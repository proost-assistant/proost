use crate::error::KernelError;
use crate::term::Term;
use crate::declaration::Declaration;
use crate::term::Term;
use crate::universe::UniverseLevel;
use derive_more::From;
use std::collections::{hash_map, HashMap};

/// Global Environment, contains the term and type of every definitions, denoted by their strings.
#[derive(Clone, Default, Debug, From, PartialEq, Eq)]
pub struct Environment(HashMap<String, Declaration>);

impl Environment {
    /// Creates an empty environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new environment binding s with (t1,t2)
    pub fn insert(&mut self, s: String, t1: Term, t2: Term) -> Result<&Self, KernelError> {
        if let hash_map::Entry::Vacant(e) = self.0.entry(s.clone()) {
            e.insert(Declaration::make(Some(t1), t2));
            Ok(self)
        } else {
            Err(KernelError::AlreadyDefined(s))
        }
    }

    /// Returns the term linked to a definition in a given environment.
    pub fn get_term(&self, s: &String, vec: &[UniverseLevel]) -> Option<Term> {
        self.0.get(s).and_then(|decl| decl.get_term(vec))
    }

    /// Returns the type linked to a definition in a given environment.
    pub fn get_type(&self, s: &String, vec: &[UniverseLevel]) -> Option<Term> {
        self.0.get(s).map(|decl| decl.get_type(vec))
    }
}
