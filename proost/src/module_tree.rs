use std::collections::VecDeque;
use std::fmt::Display;

use kernel::term::builders::Builder;

/// Type representing module tree errors.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error<'a> {
    NoModuleToClose(),

    PathNotFound(Vec<String>),

    AmbigiousPath(Vec<String>, Vec<ModuleTree<'a>>),

    BoundVariable(Vec<String>),
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;

        match self {
            NoModuleToClose() => write!(f, "no module to close"),

            PathNotFound(path) => write!(f, "{} is not a module or a public var", path.join("::")),

            AmbigiousPath(path, candidates) => {
                let res = write!(f, "{} is ambigious and could mean ", path.join("::"));

                let mut iter = candidates.iter().map(|modtree| (*modtree).get_path().join("::"));
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

impl<'a> std::error::Error for Error<'a> {}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

/// Module tree node.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ModuleNode<'a> {
    /// A module with it's parent and children positions
    Mod(&'a str, usize, Vec<usize>),

    /// A variable with it's public status
    Def(&'a str, bool),
}

/// Module tree: a tree representing module inclusions and variable private or public memberships
/// wtih a position in the tree.
///
/// A tree is made up of a vector of nodes and an indice of the vector rather than a recursive struct
/// to allow both up and down traversal of the tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleTree<'a>(&'a Vec<ModuleNode<'a>>, usize);

impl<'a> ModuleTree<'a> {
    pub fn new() -> ModuleTree<'a> {
        let node = ModuleNode::Mod("", 0, Vec::new());
        ModuleTree(&[node].to_vec(), 0)
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
    fn name(&self) -> &str {
        match self.0.get(self.1).unwrap() {
            ModuleNode::Def(name, _) => *name,
            ModuleNode::Mod(name, ..) => *name,
        }
    }

    /// Return the public status of a node.
    fn public(&self) -> bool {
        match self.0.get(self.1).unwrap() {
            ModuleNode::Def(_, public) => *public,
            ModuleNode::Mod(..) => true,
        }
    }

    /// Return the path from the root of the tree to the current module.
    pub fn get_path(&self) -> Vec<&str> {
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

        let filter = if path.is_empty() { |_: &ModuleTree| true } else { |modtree: &ModuleTree| modtree.public() };

        let candidates: Vec<ModuleTree> = self
            .children()
            .iter()
            .filter_map(|modtree| (modtree.name() == module && filter(modtree)).then(|| modtree.get_relative(path).ok()).flatten())
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
    fn add_node(&mut self, node: ModuleNode) -> ModuleTree {
        let pos = self.0.len();
        let ModuleNode::Mod(_, _, children) = self.0.get(self.1).unwrap();
        children.push(pos);
        self.0.push(node);
        ModuleTree(self.0, pos)
    }

    /// Add a definition in the current module of the tree
    pub fn define(&mut self, name: String, public: bool) -> Result<Vec<String>> {
        let res = self.get_relative([name].into());
        if res.is_ok() {
            return Err(Error::BoundVariable(res.unwrap().get_path()));
        }
        let modtree = self.add_node(ModuleNode::Def(name, public));
        Ok(modtree.get_path())
    }

    /// Create and move into a new (sub) module
    pub fn begin_module(&mut self, name: String) -> Result<()> {
        let res = self.get_relative([name].into());
        let module = {
            if res.is_ok() { res.unwrap() } else { self.add_node(ModuleNode::Mod(name, self.1, Vec::new())) }
        };
        *self = module;
        Ok(())
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
    pub fn use_module(&mut self, name: Vec<String>) -> Result<()> {
        let modtree = self.get_absolute(name.into())?;
        let ModuleNode::Mod(_, _, children) = self.0.get(self.1).unwrap();
        children.extend(modtree.children().iter().filter_map(|modtree| {
            if let ModuleNode::Def(_, false) = self.0.get(modtree.1).unwrap() {
                return None;
            }
            Some(modtree.1)
        }));
        Ok(())
    }

    pub fn search(&self, name: &[&str]) -> Result<Vec<String>> {
        let path = name.iter().map(|s| s.to_string()).collect();
        Ok(self.get_relative(path).or_else(|_| self.get_absolute(path))?.get_path())
    }

    /// Return a contextualizef version of the builder where variables names have been expended according to the given module tree
    pub fn contextualize(&self, term: &Builder) -> Result<Builder> {
        use Builder::*;
        Ok(match term {
            Prop | Type(_) => *term,
            App(box t1, box t2) => App(box self.contextualize(t1)?, box self.contextualize(t2)?),
            Abs(name, box t1, box t2) => Abs(name, box self.contextualize(t1)?, box self.contextualize(t2)?),
            Prod(name, box t1, box t2) => Prod(name, box self.contextualize(t1)?, box self.contextualize(t2)?),
            Var(name) => Var(self.search(name)?.iter().map(|s| &s[..]).collect()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::ModuleTree;

    #[test]
    fn get_empty_path() {
        let modtree = ModuleTree::new();
        let res: Vec<String> = Vec::new();
        assert_eq!(modtree.get_path(), res);
    }

    // #[test]
    // fn failed_relative_1{}

    // #[test]
    // fn failed_relative_2{}

    // #[test]
    // fn successful_variable{}

    // #[test]
    // fn failed_variable{}

    // #[test]
    // fn successful_modules{}

    // #[test]
    // fn failed_modules{}

    // #[test]
    // fn successful_use{}

    // #[test]
    // fn failed_use{}
}
