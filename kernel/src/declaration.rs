use crate::term::Term;


/// Declarations constructed through commands. A declaration describes a constant in the environment, whether it's a definition with 
/// a corresponding term, or an axiom with only a type.
/// univ_vars corresponds to the number of universe variables bound to the declaration. 
/// No universe variable can be "free" in a term, meaning that for all Var(i) in ty or term, i<univ_vars.
/// Additionally, ty and term *should* in theory always have the same number of universe variables, and as such, only a single method is needed.
/// However, additional checks to ensure this invariant will have to be put in place. For now, when constructing declarations, only the number of 
/// universes in term are counted. Since ty has to be infered, or at least checked against the infered type before being added, it has to have <= universes than term.
#[derive(Clone)]
pub struct Declaration {
    ty : Term,
    term : Option<Term>,
    univ_vars : usize
}

impl Declaration {
    
}