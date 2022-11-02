use crate::{Environment, KernelError, Term};

#[derive(Debug, Eq, PartialEq)]
pub enum Command {
    Define(String, Option<Term>, Term),

    CheckType(Term, Term),

    GetType(Term),

    Eval(Term),
}

impl Command {
    // TODO (#19)
    pub fn process(self, env: &mut Environment) -> Result<Option<Term>, KernelError> {
        match self {
            Command::Define(s, None, term) => term
                .infer(env)
                .and_then(|t| env.insert(s, term, t).map(|_| None)),

            Command::Define(s, Some(t), term) => term
                .check(&t, env)
                .and_then(|_| env.insert(s, term, t).map(|_| None)),

            Command::CheckType(t1, t2) => t1.check(&t2, env).map(|_| None),

            Command::GetType(t) => t.infer(env).map(Some),

            Command::Eval(t) => Ok(Some(t.normal_form(env))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Command, Environment, Term};

    fn simple_env() -> Environment {
        Environment::new()
            .insert("x".to_string(), Term::Type(0.into()), Term::Prop)
            .unwrap()
            .clone()
    }

    #[test]
    fn failed_untyped_define() {
        let cmd = Command::Define("x".to_string(), None, Term::Prop);
        let mut env = simple_env();

        assert!(cmd.process(&mut env).is_err());
        assert_eq!(env, simple_env());
    }

    #[test]
    fn successful_untyped_define() {
        let cmd = Command::Define("y".to_string(), None, Term::Prop);
        let mut env = simple_env();

        assert!(cmd.process(&mut env).is_ok());
        assert_eq!(
            env,
            *(simple_env()
                .insert("y".to_string(), Term::Prop, Term::Type(0.into()))
                .unwrap())
        );
    }

    #[test]
    fn failed_typed_define() {
        let cmd = Command::Define("y".to_string(), Some(Term::Type(1.into())), Term::Prop);
        let mut env = simple_env();

        assert!(cmd.process(&mut env).is_err());
        assert_eq!(env, simple_env());
    }

    #[test]
    fn successful_typed_define() {
        let cmd = Command::Define("y".to_string(), Some(Term::Type(0.into())), Term::Prop);
        let mut env = simple_env();

        assert!(cmd.process(&mut env).is_ok());
        assert_eq!(
            env,
            *(simple_env()
                .insert("y".to_string(), Term::Prop, Term::Type(0.into()))
                .unwrap())
        );
    }

    #[test]
    fn failed_checktype() {
        let cmd = Command::CheckType(Term::Prop, Term::Prop);
        let mut env = simple_env();

        assert!(cmd.process(&mut env).is_err());
        assert!(env == simple_env());
    }

    #[test]
    fn successful_checktype() {
        let cmd = Command::CheckType(Term::Prop, Term::Type(0.into()));
        let mut env = simple_env();

        assert!(cmd.process(&mut env).is_ok());
        assert_eq!(env, simple_env());
    }

    #[test]
    fn failed_gettype() {
        let cmd = Command::GetType(Term::Const("y".to_string(), Vec::new()));
        let mut env = simple_env();

        assert!(cmd.process(&mut env).is_err());
        assert!(env == simple_env());
    }

    #[test]
    fn successful_gettype() {
        let cmd = Command::GetType(Term::Prop);
        let mut env = simple_env();

        assert!(cmd.process(&mut env).is_ok());
        assert_eq!(env, simple_env());
    }
}
