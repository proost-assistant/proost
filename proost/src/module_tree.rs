use std::fmt;
use std::iter::once;

use indextree::{Arena, NodeId};
use itertools::Itertools;

use crate::error::Result;

/// Type representing module tree errors.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error<'build> {
    NoModuleToClose(),

    PathNotFound(Vec<&'build str>),

    AmbigiousPath(Vec<&'build str>, Vec<Vec<String>>),

    BoundVariable(&'build str),

    BoundModule(&'build str),
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
        let mut arena = Arena::new();
        let root = arena.new_node(ModuleNode::Module("".to_string(), true, false));
        Self {
            arena,
            root,
            position: root,
        }
    }

    /// Return the NodeId(s) corresponding to the given absolute path from the given position if it exists
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

    /// Return the NodeId(s) corresponding to the given absolute path if it exists
    fn get_absolute<'build, I>(&self, path: I) -> Vec<NodeId>
    where
        I: Iterator<Item = &'build str>,
    {
        self.get_path(self.root, path)
    }

    /// Return the NodeId(s) corresponding to the given relative path if it exists
    fn get_relative<'build, I>(&self, path: I) -> Vec<NodeId>
    where
        I: Iterator<Item = &'build str>,
    {
        self.get_path(self.position, path)
    }

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

    /// Return the position of the given position starting from the root
    fn get_position(&self, position: NodeId) -> Vec<String> {
        let mut pos = position.ancestors(&self.arena).map(|nodeid| self.arena[nodeid].get().name().clone());
        pos.next();
        let mut res: Vec<String> = pos.collect();
        res.reverse();
        res
    }

    /// Add a definition in the current module of the tree
    pub fn define<'arena, 'build>(&mut self, name: &'build str, public: bool) -> Result<'arena, 'build, ()> {
        let res = self.get_relative(once(name));
        if !res.is_empty() {
            return Err(Error::BoundVariable(name).into());
        }
        let node = self.arena.new_node(ModuleNode::Def(name.to_string(), public));
        self.position.append(node, &mut self.arena);
        Ok(())
    }

    /// Create and move into a (new) (sub) module
    pub fn begin_module<'arena, 'build>(&mut self, name: &'build str, public: bool) -> Result<'arena, 'build, ()> {
        let res = self.get_relative(once(name));
        if !res.is_empty() {
            return Err(Error::BoundModule(name).into());
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
    // TODO
}
