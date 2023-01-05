use std::iter::once;

use derive_more::Display;
use indextree::{Arena, NodeId};

use crate::error::Result;

/// Type representing module tree errors.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq, Display)]
pub enum Error {
    #[display(fmt = "no module to close")]
    NoModuleToClose(),

    #[display(fmt = "no such var: {}", "_0.join(\"::\")")]
    PathNotFound(Vec<String>),

    //#[display(fmt = "no module to close")]
    //AmbigiousPath(Vec<&'build str>, Vec<ModuleTree<'build>>),
    #[display(fmt = "bounded variable: {}", "_0.join(\"::\")")]
    BoundVariable(Vec<String>),
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

    /// Return the NodeId corresponding to the given absolute path from the given position if it exists
    fn get_path<I>(&self, position: NodeId, path: I) -> Option<NodeId>
    where
        I: Iterator<Item = String>,
    {
        let mut candidates = vec![position];

        path.for_each(|name| {
            candidates = candidates
                .iter()
                .map(|nodeid| nodeid.children(&self.arena))
                .flatten()
                .filter(|nodeid| (*self.arena[*nodeid].get().name() == name))
                .collect()
        });

        candidates.first().copied()
    }

    /// Return the NodeId corresponding to the given absolute path if it exists
    fn get_absolute<I>(&self, path: I) -> Option<NodeId>
    where
        I: Iterator<Item = String>,
    {
        self.get_path(self.root, path)
    }

    /// Return the NodeId corresponding to the given relative path if it exists
    fn get_relative<I>(&self, path: I) -> Option<NodeId>
    where
        I: Iterator<Item = String>,
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
    pub fn define(&mut self, name: &str, public: bool) -> Result<()> {
        let res = self.get_relative(once(name.to_string()));
        if res.is_some() {
            return Err(Error::BoundVariable(self.get_position(res.unwrap())).into());
        }
        let node = self.arena.new_node(ModuleNode::Def(name.to_string(), public));
        self.position.append(node, &mut self.arena);
        Ok(())
    }

    /// Create and move into a (new) (sub) module
    pub fn begin_module(&mut self, name: &str) {
        self.position = self.get_relative(once(name.to_string())).unwrap_or({
            let node = self.arena.new_node(ModuleNode::Module(name.to_string()));
            self.position.append(node, &mut self.arena);
            node
        });
    }

    /// Move out of the current module or raise an error if currently not in a module
    pub fn end_module<'arena>(&mut self) -> Result<'arena, 'static, ()> {
        let parent = self.arena[self.position].parent();
        match parent {
            None => Err(Error::NoModuleToClose().into()),
            Some(node) => {
                self.position = node;
                Ok(())
            },
        }
    }
}
