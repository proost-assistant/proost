use crate::error::KernelError;
use crate::term::Term;
use derive_more::From;
use std::collections::{hash_map, HashMap};

/// Global Environment, contains the term and type of every definitions, denoted by their strings.
#[derive(Clone, Default, From)]
pub struct Environment(HashMap<String, (Term, Term)>);

impl Environment {
    /// Creates an empty environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new environment binding s with (t1,t2)
    pub fn insert(&mut self, s: String, t1: Term, t2: Term) -> Result<(), KernelError> {
        if let hash_map::Entry::Vacant(e) = self.0.entry(s.clone()) {
            e.insert((t1, t2));
            Ok(())
        } else {
            Err(KernelError::AlreadyDefined(s))
        }
    }

    /// Returns the term linked to a definition in a given environment.
    pub fn get_term(&self, s: &String) -> Option<Term> {
        self.0.get(s).map(|(t, _)| t.clone())
    }

    /// Returns the type linked to a definition in a given environment.
    pub fn get_type(&self, s: &String) -> Option<Term> {
        self.0.get(s).map(|(_, t)| t.clone())
    }
}
