use std::collections::VecDeque;
use std::fmt::Display;

/// Type representing module tree errors.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    NoModuleToClose(),

    PathNotFound(Vec<String>),

    AmbigiousPath(Vec<String>, Vec<ModuleTree>),

    BoundVariable(Vec<String>),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            NoModuleToClose() => write!(f, "no module to close"),

            PathNotFound(path) => write!(f, "{} is not a module or a public var", path.join("::")),

            AmbigiousPath(path, candidates) => {
                let res = write!(f, "{} is ambigious and could mean ", path.join("::"));

                let iter = candidates.iter().map(|modtree| (*modtree).get_path().join("::"));
                let first = iter.next();
                let last = iter.next_back();

                res.and(write!(f, "{}", first.unwrap()));
                iter.for_each(|s| {
                    res.and(write!(f, ", {}", s));
                });
                last.and_then(|s| Some(res.and(write!(f, " or {},", s))));
                res
            },

            BoundVariable(path) => write!(f, "{} already defined", path.join("::")),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

/// Module tree node.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ModuleNode {
    /// A module with it's parent and children positions
    Mod(String, usize, Vec<usize>),

    /// A variable with it's public status
    Def(String, bool),
}

/// Module tree: a tree representing module inclusions and variable private or public memberships
/// wtih a position in the tree.
///
/// A tree is made up of a vector of nodes and an indice of the vector rather than a recursive struct
/// to allow both up and down traversal of the tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleTree(Vec<ModuleNode>, usize);

impl ModuleTree {
    pub fn new() -> ModuleTree {
        let node = ModuleNode::Mod(String::new(), 0, Vec::new());
        ModuleTree([node].to_vec(), 0)
    }

    /// Return the root of the tree.
    fn root(&self) -> ModuleTree {
        ModuleTree(self.0, 0)
    }

    /// Return the parent of a module node.
    fn parent(&self) -> ModuleTree {
        let ModuleNode::Mod(_, pos, _) = self.0.get(self.1).unwrap();
        ModuleTree(self.0, *pos)
    }

    /// Return the children of a node
    fn children(&self) -> Vec<ModuleTree> {
        match self.0.get(self.1).unwrap() {
            ModuleNode::Def(..) => Vec::new(),
            ModuleNode::Mod(.., children) => children.iter().map(|i| ModuleTree(self.0, *i)).collect(),
        }
    }

    /// Return the name of a node.
    fn name(&self) -> String {
        match self.0.get(self.1).unwrap() {
            ModuleNode::Def(name, ..) => *name,
            ModuleNode::Mod(name, ..) => *name,
        }
    }

    /// Return the path from the root of the tree to the current module.
    pub fn get_path(&self) -> Vec<String> {
        let ModuleNode::Mod(name, pos, _) = self.0.get(self.1).unwrap();
        if *pos == 0_usize {
            Vec::new()
        } else {
            let path = self.parent().get_path();
            path.push(*name);
            path
        }
    }

    /// Return the position in the tree obtained by following the given path from the root
    fn get_absolute(&self, path: VecDeque<String>) -> Result<ModuleTree> {
        self.root().get_relative(path)
    }

    /// Return the position in the tree obtained by following the given path if it exists
    fn get_relative(&self, path: VecDeque<String>) -> Result<ModuleTree> {
        if path.is_empty() {
            return Ok(*self);
        }

        let module = path.pop_front().unwrap();
        if module == "super" {
            return self.parent().get_relative(path);
        }

        let candidates: Vec<ModuleTree> = self
            .children()
            .iter()
            .filter_map(|modtree| (modtree.name() == module).then(|| modtree.get_relative(path).ok()).flatten())
            .collect();

        if candidates.is_empty() {
            return Err(Error::PathNotFound(path.into()));
        }

        if candidates.len() > 1 {
            return Err(Error::AmbigiousPath(path.into(), candidates));
        }

        Ok(candidates[0])
    }

    /// Add a node in the current module of the tree
    fn add_node(&mut self, node: ModuleNode) -> usize {
        let pos = self.0.len();
        let ModuleNode::Mod(_, _, children) = self.0.get(self.1).unwrap();
        children.push(pos);
        self.0.push(node);
        pos
    }

    /// Add a definition in the current module of the tree
    pub fn define(&mut self, name: String, public: bool) -> Result<()> {
        let res = self.get_relative([name].into());
        if res.is_ok() {
            return Err(Error::BoundVariable(res.unwrap().get_path()));
        }
        self.add_node(ModuleNode::Def(name, public));
        Ok(())
    }

    /// Create and move into a new (sub) module
    pub fn begin_module(&mut self, name: String) {
        let res = self.get_relative([name].into());
        let module_pos = {
            if res.is_ok() { res.unwrap().1 } else { self.add_node(ModuleNode::Mod(name, self.1, Vec::new())) }
        };
        self.1 = module_pos
    }

    /// Move out of the current module or raise an error if currently not in a module
    pub fn end_module(&mut self) -> Result<()> {
        let parent = self.parent();
        if *self == parent {
            return Err(Error::NoModuleToClose());
        }
        *self = parent;
        Ok(())
    }

    /// Add a use in the current module of the tree
    /// TODO manage duplicates
    pub fn use_module(&mut self, _name: Vec<String>) {}
}
