use crate::term::Term;
use crate::universe::UniverseLevel;

/// Declarations constructed through commands. A declaration describes a constant in the environment, whether it's a definition with
/// a corresponding term, or an axiom with only a type.
/// univ_vars corresponds to the number of universe variables bound to the declaration.
/// No universe variable can be "free" in a term, meaning that for all Var(i) in ty or term, i<univ_vars.
/// Additionally, ty and term *should* in theory always have the same number of universe variables, and as such, only a single method is needed.
/// However, additional checks to ensure this invariant will have to be put in place. For now, when constructing declarations, only the number of
/// universes in ty are counted.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    ty: Term,
    term: Option<Term>,
    univ_vars: usize,
}

impl Declaration {
    pub fn make(term: Option<Term>, ty: Term) -> Declaration {
        Declaration {
            ty: ty.clone(),
            term,
            univ_vars: ty.univ_vars(),
        }
    }

    pub fn get_type(&self, vec: &[UniverseLevel]) -> Term {
        if self.univ_vars != vec.len() {
            //TODO wrap in a Result monad
            panic!(
                "wrong type of universe arguments, expected {}, found {}",
                self.univ_vars,
                vec.len()
            )
        } else {
            self.ty.substitute_univs(vec)
        }
    }

    pub fn get_term(&self, vec: &[UniverseLevel]) -> Option<Term> {
        if self.univ_vars != vec.len() {
            //TODO wrap in a Result monad
            panic!(
                "wrong type of universe arguments, expected {}, found {}",
                self.univ_vars,
                vec.len()
            )
        } else {
            self.clone().term.map(|x| x.substitute_univs(vec))
        }
    }
}
