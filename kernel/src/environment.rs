use crate::term::Term;
use derive_more::{From, Into};
use std::collections::HashMap;

#[derive(Clone, From, Into, Default)]
pub struct Environment(HashMap<String, (Term, Term)>);

#[derive(Debug, Clone)]
pub enum EnvError {
    AlreadyDefined(String),
}

impl Environment {
    pub fn new() -> Environment {
        Default::default()
    }

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

    pub fn get_term(self: Environment, s: String) -> Option<Term> {
        <Environment as Into<HashMap<String, (Term, Term)>>>::into(self)
            .get(&s)
            .map(|(t, _)| t.clone())
    }
    pub fn get_type(self: Environment, s: String) -> Option<Term> {
        <Environment as Into<HashMap<String, (Term, Term)>>>::into(self)
            .get(&s)
            .map(|(_, t)| t.clone())
    }
}
