use std::cell::{Cell, RefCell};
use std::fmt;
use std::iter::once;
use std::rc::Rc;

use indextree::{Arena, DebugPrettyPrint, NodeId};
use itertools::Itertools;
use kernel::memory::term::builder::Builder;

use crate::error::Result;

/// Type representing module tree errors.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error<'build> {
    NoModuleToClose(),

    PathNotFound(Vec<&'build str>),

    AmbigiousPath(Vec<&'build str>, Vec<Vec<String>>),

    BoundVariable(String),

    BoundModule(String),
}

impl<'build> fmt::Display for Error<'build> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::{AmbigiousPath, BoundModule, BoundVariable, NoModuleToClose, PathNotFound};

        match self {
            NoModuleToClose() => write!(f, "no module to close"),

            PathNotFound(name) => write!(f, "no such var or module: {}", name.join("::")),

            AmbigiousPath(name, potential) => {
                let potentials: Vec<String> = potential.into_iter().map(|v| v.join("::")).collect();
                write!(f, "ambiguous identifier {}: {}", name.join("::"), potentials.join(", "))
            },

            BoundVariable(name) => write!(f, "bounded var: {name}"),

            BoundModule(name) => write!(f, "bounded module: {name}"),
        }
    }
}

/// Module tree node.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ModuleNode {
    /// A module with it's public and used status
    Module(String, bool, bool),

    /// A variable with it's public status
    Def(String, bool),
}

impl ModuleNode {
    /// Return the name of a node.
    fn name(&self) -> &String {
        match self {
            ModuleNode::Module(name, ..) => name,
            ModuleNode::Def(name, _) => name,
        }
    }

    /// Return the public status of a node.
    fn public(&self) -> &bool {
        match self {
            ModuleNode::Module(_, public, _) => public,
            ModuleNode::Def(_, public) => public,
        }
    }
}

/// Module tree
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleTree {
    arena: Arena<ModuleNode>,
    root: NodeId,
    position: NodeId,
}

impl ModuleTree {
    pub fn new() -> Self {
        Self::build(Arena::new(), "")
    }

    // /// Creates a ModuleTree living in another ModuleTree arena
    // pub fn subtree(self, name: &str) -> Self {
    //     Self::build(self.arena, name)
    // }

    fn build(mut arena: Arena<ModuleNode>, name: &str) -> Self {
        let root = arena.new_node(ModuleNode::Module(name.to_string(), true, false));
        Self {
            arena,
            root,
            position: root,
        }
    }

    // /// Grafts a ModuleTree living in another ModuleTree arena to the main tree
    // pub fn graft(&mut self, tree: ModuleTree) {
    //     if self.arena == tree.arena {
    //         self.position.append(tree.root, &mut self.arena)
    //     }
    // }

    /// Pretty print a node and it's descendants. Provided for debugging purpose only. Should not be used to conduct tests.
    #[allow(dead_code)]
    fn debug_pretty_print(&self) -> DebugPrettyPrint<ModuleNode> {
        self.root.debug_pretty_print(&self.arena)
    }

    /// Returns the NodeId(s) corresponding to the given absolute path from the given position if it exists
    fn get_path<'build, I>(&self, position: NodeId, path: I) -> Vec<NodeId>
    where
        I: Iterator<Item = &'build str>,
    {
        let mut candidates = vec![position];
        let mut allow_private = true;

        path.for_each(|name| {
            if name != "self" {
                if name == "super" {
                    candidates = candidates
                        .iter()
                        .filter_map(|nodeid| {
                            let mut iter = nodeid.ancestors(&self.arena);
                            iter.next();
                            iter.next()
                        })
                        .unique()
                        .collect();
                } else {
                    candidates = candidates
                        .iter()
                        .map(|nodeid| nodeid.children(&self.arena))
                        .flatten()
                        .filter(|nodeid| {
                            let content = self.arena[*nodeid].get();
                            content.name() == name && (*content.public() || allow_private)
                        })
                        .collect();

                    allow_private = false;
                }
            }
        });

        candidates
    }

    /// Returns the NodeId(s) corresponding to the given absolute path if it exists
    fn get_absolute<'build, I>(&self, path: I) -> Vec<NodeId>
    where
        I: Iterator<Item = &'build str>,
    {
        self.get_path(self.root, path)
    }

    /// Returns the NodeId(s) corresponding to the given relative path if it exists
    fn get_relative<'build, I>(&self, path: I) -> Vec<NodeId>
    where
        I: Iterator<Item = &'build str>,
    {
        self.get_path(self.position, path)
    }

    /// Returns the unique NodeId corresponding to the given path
    ///
    /// Paths can be relative or absolute depending on the use of "super" and "self" keywords
    fn get_identifier<'arena, 'build>(&self, path: &Vec<&'build str>) -> Result<'arena, 'build, NodeId> {
        let candidates = if let Some(s) = path.first()
            && (*s == "super" || *s == "self") {
                self.get_relative(path.iter().copied())
        } else {
            self.get_absolute(path.iter().copied())
        };

        if candidates.is_empty() {
            return Err(Error::PathNotFound(path.clone()).into());
        }

        if candidates.len() > 1 {
            return Err(
                Error::AmbigiousPath(path.clone(), candidates.iter().map(|nodeid| self.get_position(*nodeid)).collect()).into()
            );
        }

        Ok(*candidates.first().unwrap())
    }

    /// Returns the position of the given position starting from the root
    fn get_position(&self, position: NodeId) -> Vec<String> {
        let mut pos = position.ancestors(&self.arena).map(|nodeid| self.arena[nodeid].get().name().clone());
        pos.next();
        let mut res: Vec<String> = pos.collect();
        res.reverse();
        res
    }

    /// Add a definition in the current module of the tree
    pub fn define<'arena, 'build>(&mut self, name: &'build str, public: bool) -> Result<'arena, 'build, NodeId> {
        let res = self.get_relative(once(name));
        if !res.is_empty() {
            return Err(Error::BoundVariable(name.to_string()).into());
        }
        let node = self.arena.new_node(ModuleNode::Def(name.to_string(), public));
        self.position.append(node, &mut self.arena);
        Ok(node)
    }

    /// Create and move into a (new) (sub) module
    pub fn begin_module<'arena, 'build>(&mut self, name: &'build str, public: bool) -> Result<'arena, 'build, ()> {
        if !self.get_relative(once(name)).is_empty() {
            return Err(Error::BoundModule(name.to_string()).into());
        }
        let node = self.arena.new_node(ModuleNode::Module(name.to_string(), public, false));
        self.position.append(node, &mut self.arena);
        self.position = node;
        Ok(())
    }

    /// Move out of the current module or raise an error if currently not in a module
    pub fn end_module<'arena, 'build>(&mut self) -> Result<'arena, 'build, ()> {
        let parent = self.arena[self.position].parent();
        match parent {
            None => Err(Error::NoModuleToClose().into()),
            Some(node) => {
                self.position = node;
                Ok(())
            },
        }
    }

    /// Use a module or a variable identified by its path if it exists
    pub fn use_module<'arena, 'build>(&mut self, path: &Vec<&'build str>, public: bool) -> Result<'arena, 'build, ()> {
        let nodeid = self.get_identifier(path)?;

        match self.arena[nodeid].get() {
            // The obtained node is a module that was not defined here but imported through the use command
            ModuleNode::Module(.., true) => Err(Error::PathNotFound(path.clone()).into()),

            // The obtained node is a module
            ModuleNode::Module(name, _, false) => {
                if !self.get_relative(once(name.as_str())).is_empty() {
                    return Err(Error::BoundModule(name.clone()).into());
                }

                let new_module = self.arena.new_node(ModuleNode::Module(name.clone(), public, true));
                self.position.append(new_module, &mut self.arena);

                let new_children: Vec<NodeId> = nodeid
                    .children(&self.arena)
                    .filter(|nodeid_| *self.arena[*nodeid_].get().public() == true)
                    .collect();
                new_children.iter().for_each(|nodeid_| new_module.append(*nodeid_, &mut self.arena));
                Ok(())
            },

            // The obtained node is a variable
            ModuleNode::Def(name, _) => {
                if !self.get_relative(once(name.as_str())).is_empty() {
                    return Err(Error::BoundVariable(name.clone()).into());
                }

                let new_def = self.arena.new_node(ModuleNode::Def(name.clone(), public));
                self.position.append(new_def, &mut self.arena);
                Ok(())
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let mut tree = ModuleTree::new();

        assert!(tree.define("x", true).is_ok());
        assert!(tree.define("x", false).is_err());
        assert!(tree.define("y", false).is_ok());
        assert!(tree.end_module().is_err());
        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.define("x", true).is_ok());
        assert!(tree.define("x", false).is_err());
        assert!(tree.define("y", false).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.define("x", true).is_err());
        assert!(tree.define("w", true).is_ok());
        assert!(tree.begin_module("mod1", false).is_err());
        assert!(tree.begin_module("mod2", false).is_ok());
        assert!(tree.begin_module("mod3", true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.end_module().is_err());
    }

    #[test]
    fn use_modules_existing() {
        let mut tree = ModuleTree::new();

        assert!(tree.define("x", true).is_ok());
        assert!(tree.define("y", false).is_ok());
        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.define("x1", true).is_ok());
        assert!(tree.define("y1", false).is_ok());
        assert!(tree.begin_module("mod2", false).is_ok());
        assert!(tree.define("x2", true).is_ok());
        assert!(tree.define("y2", false).is_ok());
        assert!(tree.end_module().is_ok());

        assert!(tree.use_module(&vec!["x1"], false).is_err());
        assert!(tree.use_module(&vec!["y1"], false).is_err());
        assert!(tree.use_module(&vec!["self", "x1"], false).is_err());
        assert!(tree.use_module(&vec!["self", "self", "x1"], false).is_err());
        assert!(tree.use_module(&vec!["mod2", "x2"], false).is_err());
        assert!(tree.use_module(&vec!["mod2", "y2"], false).is_err());
        assert!(tree.use_module(&vec!["super", "x"], false).is_ok());
        assert!(tree.use_module(&vec!["super", "x"], false).is_err());
        assert!(tree.use_module(&vec!["self", "super", "y"], false).is_ok());
        assert!(tree.end_module().is_ok());

        assert!(tree.use_module(&vec!["x"], false).is_err());
        assert!(tree.use_module(&vec!["super"], false).is_err());
        assert!(tree.use_module(&vec!["mod1", "x1"], false).is_ok());
        assert!(tree.use_module(&vec!["mod1", "x1"], false).is_err());
        assert!(tree.use_module(&vec!["mod1", "y1"], false).is_err());
        assert!(tree.use_module(&vec!["mod1", "x"], false).is_err());

        assert!(tree.use_module(&vec!["mod1", "mod2", "x2"], false).is_err());
        assert!(tree.use_module(&vec!["mod1", "mod2", "x2"], false).is_err());
    }

    #[test]
    fn use_module_unexisting() {
        let mut tree = ModuleTree::new();

        assert!(tree.define("x", true).is_ok());
        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.use_module(&vec!["mod1", "x"], false).is_err());
        assert!(tree.use_module(&vec!["mod1", "mod2", "x"], false).is_err());
    }

    #[test]
    fn use_module_chain() {
        let mut tree = ModuleTree::new();

        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.begin_module("mod2", true).is_ok());
        assert!(tree.begin_module("mod3", true).is_ok());
        assert!(tree.define("x", true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.use_module(&vec!["mod3", "x"], true).is_err());
        assert!(tree.use_module(&vec!["self", "mod3", "x"], true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.use_module(&vec!["self", "mod2", "x"], true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.use_module(&vec!["self", "mod1", "x"], true).is_ok());
    }

    #[test]
    fn use_module_super() {
        let mut tree = ModuleTree::new();

        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.begin_module("mod2", true).is_ok());
        assert!(tree.begin_module("mod3", true).is_ok());
        assert!(tree.define("x", true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.use_module(&vec!["super", "super", "mod1", "mod2", "mod3", "x"], true).is_ok());
    }

    #[test]
    fn use_module_absolute() {
        let mut tree = ModuleTree::new();

        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.begin_module("mod2", true).is_ok());
        assert!(tree.begin_module("mod3", true).is_ok());
        assert!(tree.define("x", true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.use_module(&vec!["mod1", "mod2", "mod3", "x"], true).is_ok());
    }
}
