use super::arena::Term;
use super::level::Level;

/// Declarations constructed through commands. A declaration describes a constant in the environment, whether it's a definition with
/// a corresponding term, or an axiom with only a type.
/// univ_vars corresponds to the number of universe variables bound to the declaration.
/// No universe variable can be "free" in a term, meaning that for all Var(i) in ty or term, i<univ_vars.
/// Additionally, ty and term *should* in theory always have the same number of universe variables, and as such, only a single method is needed.
/// However, additional checks to ensure this invariant will have to be put in place. For now, when constructing declarations, only the number of
/// universes in ty are counted.
#[derive(Clone, Debug, PartialEq, Eq)]
 pub struct Declaration<'arena> {
    ty: Term<'arena>,
    term: Option<Term<'arena>>,
    univ_vars: usize,
}

impl<'arena> Declaration<'arena> {
    pub fn make(term: Option<Term>, ty: Term) -> Self {
        Declaration {
            ty: ty.clone(),
            term,
            univ_vars: ty.univ_vars(),
        }
    }

    /// Returns the type linked to a definition in a given environment.
    pub fn get_type(&self, vec: &[Level<'arena>]) -> Result<Term<'arena>, KernelError> {
        if self.univ_vars != vec.len() {
            Err(KernelError::WrongNumberOfUniverseArguments(
                self.univ_vars,
                vec.len(),
            ))
        } else {
            Ok(self.ty.substitute_univs(vec))
        }
    }

    /// Returns the type linked to a definition in a given environment.
    pub fn get_type_free_univ(&self) -> Term {
        self.ty.clone()
    }

    /// Returns the term linked to a definition in a given environment.
    /// Since the declaration might be an axiom, it might not have an associated term to reduce to, hence the Option.
    pub fn get_term(&self, vec: &[UniverseLevel]) -> Result<Option<Term>, KernelError> {
        if self.univ_vars != vec.len() {
            Err(KernelError::WrongNumberOfUniverseArguments(
                self.univ_vars,
                vec.len(),
            ))
        } else {
            Ok(self.clone().term.map(|x| x.substitute_univs(vec)))
        }
    }
}
