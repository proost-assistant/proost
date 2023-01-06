use std::fmt;
use std::iter::once;

use derive_more::Display;
use indextree::{Arena, NodeId};
use itertools::Itertools;

use crate::error::Result;

/// Type representing module tree errors.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error<'build> {
    NoModuleToClose(),

    PathNotFound(Vec<String>),

    AmbigiousPath(Vec<&'build str>, Vec<Vec<String>>),

    BoundVariable(&'build str),

    BoundModule(&'build str),
}

impl<'build> fmt::Display for Error<'build> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::{AmbigiousPath, BoundModule, BoundVariable, NoModuleToClose, PathNotFound};

        match self {
            NoModuleToClose() => write!(f, "no module to close"),

            PathNotFound(name) => write!(f, "no such var: {}", name.join("::")),

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
    /// A module with it's parent and children positions
    Module(String),

    /// A variable with it's public status
    Def(String, bool),
}

impl ModuleNode {
    /// Return the name of a node.
    fn name(&self) -> &String {
        match self {
            ModuleNode::Module(name) => name,
            ModuleNode::Def(name, _) => name,
        }
    }

    /// Return the public status of a node.
    fn public(&self) -> bool {
        match self {
            ModuleNode::Module(_) => true,
            ModuleNode::Def(_, public) => *public,
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
        let root = arena.new_node(ModuleNode::Module("".to_string()));
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

        path.for_each(|name| {
            if name != "self" {
                candidates = if name == "super" {
                    candidates
                        .iter()
                        .filter_map(|nodeid| {
                            let mut iter = nodeid.ancestors(&self.arena);
                            iter.next();
                            iter.next()
                        })
                        .unique()
                        .collect()
                } else {
                    candidates
                        .iter()
                        .map(|nodeid| nodeid.children(&self.arena))
                        .flatten()
                        .filter(|nodeid| (*self.arena[*nodeid].get().name() == name))
                        .collect()
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
    pub fn begin_module<'arena, 'build>(&mut self, name: &'build str) -> Result<'arena, 'build, ()> {
        let res = self.get_relative(once(name));
        if !res.is_empty() {
            return Err(Error::BoundModule(name).into());
        }
        let node = self.arena.new_node(ModuleNode::Module(name.to_string()));
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
    pub fn use_path<'arena, 'build, I>(&mut self, path: I) -> Result<'arena, 'build, ()>
    where
        I: Iterator<Item = &'build str>,
    {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // TODO
}
