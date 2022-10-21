use crate::term::Term;
use derive_more::{From, Into};
use std::collections::HashMap;

/// Global Environment, contains the term and type of every definitions, denoted by their strings.
#[derive(Clone, From, Default)]
pub struct Environment(HashMap<String, (Term, Term)>);

#[derive(Debug, Clone)]
pub enum EnvError {
    AlreadyDefined(String),
}

impl Environment {
    /// Creates an empty environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new environment binding s with (t,ty)
    pub fn insert(self, s: String, t: Term, ty: Term) -> Result<Self, EnvError> {
        match self.0.clone().get(&s) {
            Some(_) => Err(EnvError::AlreadyDefined(s)),
            None => {
                let mut res = self.0;

                res.insert(s, (t, ty));
                Ok(res.into())
            }
        }
    }

    /// Returns the term linked to a definition in a given environment.
    pub fn get_term(self, s: String) -> Option<Term> {
        self.0.get(&s).map(|(t, _)| t.clone())
    }

    /// Returns the type linked to a definition in a given environment.
    pub fn get_type(self, s: String) -> Option<Term> {
        self.0.get(&s).map(|(_, t)| t.clone())
    }
}
