use crate::error::{Error, Result};
use crate::term::Term;
use derive_more::{Deref, DerefMut, Display};
use std::collections::{hash_map, HashMap};

/// Global Environment, contains the term and type of every definitions, denoted by their strings.
#[derive(Clone, Default, Debug, Deref, DerefMut, Eq, PartialEq)]
pub struct Environment(HashMap<String, (Term, Term)>);

/// Errors that can occur, at runtime, during environment manipulation.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum EnvironmentError {
    #[display(fmt = "{} is already defined", _0)]
    AlreadyDefined(String),

    #[display(fmt = "variable {} is not found", _0)]
    VariableNotFound(String),
}

impl Environment {
    /// Creates an empty environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new environment binding s with (t1,t2)
    pub(crate) fn insert(&mut self, s: String, t1: Term, t2: Term) -> Result<&Self> {
        if let hash_map::Entry::Vacant(e) = self.entry(s.clone()) {
            e.insert((t1, t2));
            Ok(self)
        } else {
            Err(Error {
                kind: EnvironmentError::AlreadyDefined(s).into(),
            })
        }
    }

    /// Returns the term linked to a definition in a given environment.
    pub(crate) fn get_term(&self, s: &String) -> Option<Term> {
        self.get(s).map(|(t, _)| t.clone())
    }

    /// Returns the type linked to a definition in a given environment.
    pub(crate) fn get_type(&self, s: &String) -> Option<Term> {
        self.get(s).map(|(_, t)| t.clone())
    }
}
