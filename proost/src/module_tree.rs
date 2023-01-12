use std::collections::HashMap;
use std::fmt;
use std::iter::once;

use indextree::{Arena, DebugPrettyPrint, NodeId};
use itertools::Itertools;
use kernel::memory::arena::Id;

use crate::error::Result;

/// Type representing module tree errors.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error<'build> {
    NoModuleToClose(),

    PathNotFound(Vec<&'build str>),

    FileNotFound(String),

    //AmbigiousPath(Vec<&'build str>, Vec<Vec<String>>), //TODO no needed
    BoundIdentifier(String),
}

impl<'build> fmt::Display for Error<'build> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::{BoundIdentifier, FileNotFound, NoModuleToClose, PathNotFound};

        match self {
            NoModuleToClose() => write!(f, "no module to close"),

            PathNotFound(name) => write!(f, "no such var or module: {}", name.join("::")),

            // AmbigiousPath(name, potential) => {
            //     let potentials: Vec<String> = potential.into_iter().map(|v| v.join("::")).collect();
            //     write!(f, "ambiguous identifier {}: {}", name.join("::"), potentials.join(", "))
            // },
            BoundIdentifier(name) => write!(f, "bounded identifier: {name}"),

            FileNotFound(name) => write!(f, "unknow file: {name}"),
        }
    }
}

/// Module tree node.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ModuleNode {
    /// A module with it's public and used status
    Module(String, bool),

    /// A variable with it's public status
    Def(String, bool),
}

impl ModuleNode {
    /// Return the name of a node.
    fn name(&self) -> &String {
        match self {
            Self::Module(name, _) => name,
            Self::Def(name, _) => name,
        }
    }

    /// Return the public status of a node.
    fn public(&self) -> &bool {
        match self {
            Self::Module(_, public) => public,
            Self::Def(_, public) => public,
        }
    }
}

/// Module tree
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleTree {
    arena: Arena<ModuleNode>,
    root: Vec<NodeId>,
    position: Vec<NodeId>,
    imported: HashMap<String, NodeId>,
}

impl ModuleTree {
    pub fn new() -> Self {
        let mut arena = Arena::new();
        let root = arena.new_node(ModuleNode::Module(String::new(), true));
        Self {
            arena,
            root: vec![root],
            position: vec![root],
            imported: HashMap::new(),
        }
    }

    /// Pretty print a node and it's children. Provided for debugging purpose only. Should not be used to conduct tests.
    #[allow(dead_code)]
    fn debug_pretty_print(&self) -> DebugPrettyPrint<ModuleNode> {
        self.root.last().unwrap().debug_pretty_print(&self.arena)
    }

    /// Copy a public nodeid and it's public children
    fn copy_public(&mut self, nodeid: NodeId) -> Option<NodeId> {
        match self.arena[nodeid].get() {
            ModuleNode::Module(_, false) | ModuleNode::Def(_, false) => None,
            ModuleNode::Def(name, true) => Some(self.arena.new_node(ModuleNode::Def(name.clone(), true))),
            ModuleNode::Module(name, true) => {
                let parent = self.arena.new_node(ModuleNode::Module(name.clone(), true));
                let mut children: Vec<NodeId> = nodeid.children(&self.arena).collect();

                children = children.iter().filter_map(|child| self.copy_public(*child)).collect();
                children.iter().for_each(|child| parent.append(*child, &mut self.arena));

                Some(parent)
            },
        }
    }

    /// Copy a nodeid and it's public children
    fn copy(&mut self, nodeid: NodeId, public: bool) -> NodeId {
        match self.arena[nodeid].get() {
            ModuleNode::Def(name, _) => self.arena.new_node(ModuleNode::Def(name.clone(), public)),
            ModuleNode::Module(name, _) => {
                let parent = self.arena.new_node(ModuleNode::Module(name.clone(), public));
                let mut children: Vec<NodeId> = nodeid.children(&self.arena).collect();

                children = children.iter().filter_map(|child| self.copy_public(*child)).collect();
                children.iter().for_each(|child| parent.append(*child, &mut self.arena));

                parent
            },
        }
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
                        .flat_map(|nodeid| nodeid.children(&self.arena))
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
        self.get_path(*self.root.last().unwrap(), path)
    }

    /// Returns the NodeId(s) corresponding to the given relative path if it exists
    fn get_relative<'build, I>(&self, path: I) -> Vec<NodeId>
    where
        I: Iterator<Item = &'build str>,
    {
        self.get_path(*self.position.last().unwrap(), path)
    }

    /// Returns the unique `NodeId` corresponding to the given path
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

        // if candidates.len() > 1 {
        //     return Err(
        //         Error::AmbigiousPath(path.clone(), candidates.iter().map(|nodeid| self.get_position(*nodeid)).collect()).into()
        //     );
        // }

        Ok(*candidates.first().unwrap())
    }

    pub fn convert<'build>(&self) -> impl Fn(&Vec<&'build str>) -> Option<Id<'build>> + '_ {
        |path| self.get_identifier(path).ok().map(|nodeid| Into::<usize>::into(nodeid).into())
    }

    // /// Returns the position of the given position starting from the root
    // fn get_position(&self, position: NodeId) -> Vec<String> {
    //     let mut res: Vec<String> = position
    //         .ancestors(&self.arena)
    //         .map(|nodeid| self.arena[nodeid].get().name().clone())
    //         .collect();

    //     res.pop();
    //     res.reverse();
    //     res
    // }

    /// Add a definition in the current module of the tree
    pub fn define<'arena, 'build>(&mut self, name: &'build str, public: bool) -> Result<'arena, 'build, Id> {
        if !self.get_relative(once(name)).is_empty() {
            return Err(Error::BoundIdentifier(name.to_string()).into());
        }

        let nodeid = self.arena.new_node(ModuleNode::Def(name.to_string(), public));
        self.position.last().unwrap().append(nodeid, &mut self.arena);

        Ok(Into::<usize>::into(nodeid).into())
    }

    /// Create and move into a (new) (sub) module
    pub fn begin_module<'arena, 'build>(&mut self, name: &'build str, public: bool) -> Result<'arena, 'build, ()> {
        if !self.get_relative(once(name)).is_empty() {
            return Err(Error::BoundIdentifier(name.to_string()).into());
        }

        let node = self.arena.new_node(ModuleNode::Module(name.to_string(), public));
        self.position.last().unwrap().append(node, &mut self.arena);

        let indice = self.position.len() - 1;
        self.position[indice] = node;

        Ok(())
    }

    /// Move out of the current module or raise an error if currently not in a module
    pub fn end_module<'arena, 'build>(&mut self) -> Result<'arena, 'build, ()> {
        let parent = self.arena[*self.position.last().unwrap()].parent();
        match parent {
            None => Err(Error::NoModuleToClose().into()),
            Some(node) => {
                let indice = self.position.len() - 1;
                self.position[indice] = node;
                Ok(())
            },
        }
    }

    /// Use a module or a variable identified by its path if it exists
    pub fn use_module<'arena, 'build>(&mut self, path: &Vec<&'build str>, public: bool) -> Result<'arena, 'build, ()> {
        let nodeid = self.get_identifier(path)?;

        let name = self.arena[nodeid].get().name();
        if !self.get_relative(once(name.as_str())).is_empty() {
            return Err(Error::BoundIdentifier(name.clone()).into());
        }

        let new_nodeid = self.copy(nodeid, public);
        self.position.last().unwrap().append(new_nodeid, &mut self.arena);

        Ok(())
    }

    /// Begin importing a new file structure into the tree
    pub fn begin_import_file_structure(&mut self, name: String) {
        let module = self.arena.new_node(ModuleNode::Module(name.to_string(), true));
        self.root.push(module);
        self.position.push(module);
        self.imported.insert(name, module);
    }

    /// Finish importing a new file structure into the tree
    pub fn end_import_file_structure(&mut self) {
        self.position.pop();
        let module = self.root.pop().unwrap();
        self.position.last().unwrap().append(module, &mut self.arena);
    }

    /// Load a know file structure into the tree
    pub fn retrieve_file_structure(&mut self, name: String) -> Result<()> {
        let module = self.imported.get(&name).ok_or(Error::FileNotFound(name.clone()))?;
        let new_module = self.copy(*module, true);
        self.position.last().unwrap().append(new_module, &mut self.arena);
        Ok(())
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
        //assert_eq!(tree.get_position(tree.position), Vec::<String>::new());
        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.define("x", true).is_ok());
        assert!(tree.define("x", false).is_err());
        assert!(tree.define("y", false).is_ok());
        //assert_eq!(tree.get_position(tree.position), vec!["mod1"]);
        assert!(tree.end_module().is_ok());
        assert!(tree.define("x", true).is_err());
        assert!(tree.define("w", true).is_ok());
        assert!(tree.begin_module("mod1", false).is_err());
        assert!(tree.begin_module("mod2", false).is_ok());
        assert!(tree.begin_module("mod3", true).is_ok());
        //assert_eq!(tree.get_position(tree.position), vec!["mod2", "mod3"]);
        assert!(tree.end_module().is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.end_module().is_err());
    }

    #[test]
    fn use_module_existing_def() {
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
        assert!(tree.use_module(&vec!["mod1", "mod2"], false).is_err());
    }

    #[test]
    fn use_module_chain_def() {
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
    fn use_module_super_def() {
        let mut tree = ModuleTree::new();

        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.begin_module("mod2", true).is_ok());
        assert!(tree.begin_module("mod3", true).is_ok());
        assert!(tree.define("x", true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.use_module(&vec!["super", "super", "mod1", "mod2", "mod3", "x"], true).is_ok());
    }

    #[test]
    fn use_module_absolute_def() {
        let mut tree = ModuleTree::new();

        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.begin_module("mod2", true).is_ok());
        assert!(tree.begin_module("mod3", true).is_ok());
        assert!(tree.define("x", true).is_ok());
        assert!(tree.end_module().is_ok());
        assert!(tree.use_module(&vec!["mod1", "mod2", "mod3", "x"], true).is_ok());
    }

    #[test]
    fn use_module_self() {
        let mut tree = ModuleTree::new();

        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.use_module(&vec!["super", "mod1"], false).is_ok());
        assert!(tree.position.last().unwrap().children(&tree.arena).next().is_some());

        let mut tree = ModuleTree::new();

        assert!(tree.begin_module("mod1", true).is_ok());
        assert!(tree.define("x", true).is_ok());
        assert!(tree.define("y", true).is_ok());

        assert!(tree.use_module(&vec!["super", "mod1"], false).is_ok());
        assert_eq!(tree.position.last().unwrap().children(&tree.arena).count(), 3);
    }
}
