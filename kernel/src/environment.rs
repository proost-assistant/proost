use crate::term::Term;
use derive_more::{From, Into};
use std::collections::HashMap;

/// Global Environment, contains the term and type of every definitions, denoted by their strings.
#[derive(Clone, From, Into, Default)]
pub struct Environment(HashMap<String, (Term, Term)>);

#[derive(Debug, Clone)]
pub enum EnvError {
    AlreadyDefined(String),
}

impl Environment {
    /// Creates an empty environment.
    pub fn new() -> Environment {
        Default::default()
    }

    /// Creates a new environment binding s with (t,ty)
    pub fn insert(
        self: Environment,
        s: String,
        t: Term,
        ty: Term,
    ) -> Result<Environment, EnvError> {
        match <Environment as Into<HashMap<String, (Term, Term)>>>::into(self.clone()).get(&s) {
            Some(_) => Err(EnvError::AlreadyDefined(s.clone())),
            None => {
                let mut res = <Environment as Into<HashMap<String, (Term, Term)>>>::into(self);
                res.insert(s.clone(), (t, ty));
                Ok(res.into())
            }
        }
    }

    /// Returns the term linked to a definition in a given environment.
    pub fn get_term(self: Environment, s: String) -> Option<Term> {
        <Environment as Into<HashMap<String, (Term, Term)>>>::into(self)
            .get(&s)
            .map(|(t, _)| t.clone())
    }

    /// Returns the type linked to a definition in a given environment.
    pub fn get_type(self: Environment, s: String) -> Option<Term> {
        <Environment as Into<HashMap<String, (Term, Term)>>>::into(self)
            .get(&s)
            .map(|(_, t)| t.clone())
    }
}
