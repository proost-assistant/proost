use crate::{Environment, KernelError, Term};
use derive_more::Display;

#[derive(Debug, Display, Eq, PartialEq)]
pub enum Command {
    #[display(fmt = "define {} := {}.", _0, _1)]
    Define(String, Term),

    #[display(fmt = "check {} : {}.", _0, _1)]
    CheckType(Term, Term),

    #[display(fmt = "type {}.", _0)]
    GetType(Term),

    #[display(fmt = "define {} : {} := {}.", _0, _1, _2)]
    DefineCheckType(String, Term, Term),
}

impl Command {
    // TODO (#19)
    pub fn process(self, env: &mut Environment) -> Result<Option<Term>, KernelError> {
        match self {
            Command::CheckType(t1, t2) => t1.check(&t2, env).map(|_| None),
            Command::GetType(t) => t.infer(env).map(Some),
            Command::Define(s, t1) => t1
                .infer(env)
                .and_then(|t2| env.insert(s, t1, t2).map(|_| None)),
            Command::DefineCheckType(s, t1, t2) => t2
                .check(&t1, env)
                .and_then(|_| env.insert(s, t1, t2).map(|_| None)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{num_bigint::BigUint, Command, Environment, KernelError, Term};

    #[test]
    fn checktype() {
        let mut env = Environment::new();
        assert_eq!(
            Command::CheckType(Term::Type(BigUint::from(1_u64).into()), Term::Prop)
                .process(&mut env),
            Err(KernelError::TypeMismatch(
                Term::Type(BigUint::from(2_u64).into()),
                Term::Prop
            ))
        );
    }

    #[test]
    fn env() {
        let mut env = Environment::new();
        assert_eq!(
            Command::Define("x".to_string(), Term::App(box Term::Prop, box Term::Prop))
                .process(&mut env),
            Err(KernelError::NotAFunction(
                Term::Prop,
                Term::Type(BigUint::from(0_u64).into()),
                Term::Prop
            ))
        );
        // The environment was not changed, as seen with the following test.
        assert_eq!(
            Command::Define("x".to_string(), Term::Prop).process(&mut env),
            Ok(None)
        );
        assert_eq!(
            Command::DefineCheckType(
                "y".to_string(),
                Term::Type(BigUint::from(2_u64).into()),
                Term::Type(BigUint::from(1_u64).into())
            )
            .process(&mut env),
            Ok(None)
        );
        // The environment was successfully changed, as seen with the following tests.
        assert_eq!(
            Command::GetType(Term::Const("y".to_string())).process(&mut env),
            Ok(Some(Term::Type(BigUint::from(1_u64).into())))
        );
        assert_eq!(
            Command::GetType(Term::Const("x".to_string())).process(&mut env),
            Ok(Some(Term::Type(BigUint::from(0_u64).into())))
        );
        assert_eq!(
            Command::Define("x".to_string(), Term::Type(BigUint::from(0_u64).into()))
                .process(&mut env),
            Err(KernelError::AlreadyDefined("x".to_string()))
        );
        assert_eq!(
            Command::DefineCheckType(
                "x".to_string(),
                Term::Type(BigUint::from(1_u64).into()),
                Term::Type(BigUint::from(0_u64).into())
            )
            .process(&mut env),
            Err(KernelError::AlreadyDefined("x".to_string()))
        );
        // Furthermore, the previous commands did not alter the content of the environment,
        // as seen with the following test.
        assert_eq!(
            Command::GetType(Term::Const("x".to_string())).process(&mut env),
            Ok(Some(Term::Type(BigUint::from(0_u64).into())))
        );
    }
}
