//! Pretty-printing of terms.

use core::fmt;

use super::Payload::{Abs, App, Axiom, Decl, Prod, Sort, Var, Match};

/// Thin wrapper used internally to print a term associated to a (classic) identifier as a letter.
struct PrettyVar(usize);

impl fmt::Display for PrettyVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let alphabet = b"abcdefghijklmnopqrstuvwxyz".as_slice();
        match alphabet.get(self.0).copied() {
            Some(letter) => write!(f, "{}", char::from(letter)),
            None => write!(f, "x{}", self.0 - alphabet.len()),
        }
    }
}

impl<'arena> fmt::Display for super::Term<'arena> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.pretty_print(f, 0, 0, false)
    }
}

/// A term that is ready to be pretty-printed with named variables. This term must be closed.
pub struct Term<'arena>(pub super::Term<'arena>);

impl<'arena> fmt::Display for Term<'arena> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.pretty_print(f, 0, 0, true)
    }
}

impl<'arena> super::Term<'arena> {
    /// Indicates whether a term is an application.
    fn is_app(self) -> bool {
        matches!(*self, App(..))
    }

    /// Indicates whether a term is a binder, which also entails specific printing rules.
    fn is_binder(self) -> bool {
        matches!(*self, Abs(..) | Prod(..))
    }

    /// This function generates the pretty print of a term.
    ///
    /// `depth` parameter indicates the number of existing variables (number of Abs/Prop from the root of the term to the
    /// considered subterm).
    /// `distance` parameter corresponds to the number of Abs/Prop between the environement where the variables live and these
    /// variables.
    /// `is_root_closed` indicates if the root is certain to be closed. If true, the De Bruijn indexes will not appear but
    /// transformed in named variables instead.
    #[no_coverage]
    fn pretty_print(self, f: &mut fmt::Formatter, depth: usize, distance: usize, mut is_root_closed: bool) -> fmt::Result {
        is_root_closed |= self.is_certainly_closed();

        match *self {
            Var(index, _) => {
                if is_root_closed {
                    write!(f, "{}", PrettyVar(depth - distance - index.0))
                } else {
                    write!(f, "{index}")
                }
            },
            Sort(level) => match level.to_numeral() {
                Some(n) => match n {
                    0 => write!(f, "Prop"),
                    1 => write!(f, "Type"),
                    _ => write!(f, "Type {}", n - 1),
                },
                None => write!(f, "Sort {level}"),
            },
            App(fun, arg) => {
                if fun.is_binder() {
                    write!(f, "(")?;
                    fun.pretty_print(f, depth, distance, is_root_closed)?;
                    write!(f, ")")?;
                } else {
                    fun.pretty_print(f, depth, distance, is_root_closed)?;
                }
                write!(f, " ")?;
                if arg.is_app() || arg.is_binder() {
                    write!(f, "(")?;
                    arg.pretty_print(f, depth, distance, is_root_closed)?;
                    write!(f, ")")
                } else {
                    arg.pretty_print(f, depth, distance, is_root_closed)
                }
            },
            Abs(argtype, body) => {
                write!(f, "\u{003BB} ")?;
                if is_root_closed {
                    write!(f, "{}: ", PrettyVar(depth))?;
                };
                argtype.pretty_print(f, depth + 1, distance + 1, is_root_closed)?;
                write!(f, " => ")?;
                body.pretty_print(f, depth + 1, distance, is_root_closed)
            },
            Prod(argtype, body) => {
                if is_root_closed {
                    write!(f, "({}: ", PrettyVar(depth))?;
                };
                argtype.pretty_print(f, depth + 1, distance + 1, is_root_closed)?;
                if is_root_closed {
                    write!(f, ")")?;
                };
                write!(f, " -> ")?;
                body.pretty_print(f, depth + 1, distance, is_root_closed)
            },
            Decl(decl) => write!(f, "{decl}"),
            Axiom(s, _) => write!(f, "{s}"),
            Match(_, _) => todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::memory::arena::{use_arena, use_arena_with_axioms};
    use crate::memory::declaration::{Declaration, InstantiatedDeclaration};
    use crate::memory::level::builder::raw as level;
    use crate::memory::term::builder::raw::*;
    use crate::memory::term::{pretty, Term};

    #[test]
    fn display_1() {
        use_arena(|arena| {
            let decl = InstantiatedDeclaration::instantiate(Declaration(Term::prop(arena), 0), &Vec::new(), arena);
            let prop = Term::decl(decl, arena);

            assert_eq!(prop.to_string(), "(Prop).{}");
        });
    }

    #[test]
    fn display_2() {
        use_arena(|arena| {
            let lvl = level::max(level::succ(level::var(0)), level::succ(level::var(1)));

            let term = arena.build_term_raw(abs(
                sort_(lvl),
                abs(
                    type_usize(0),
                    abs(
                        type_usize(1),
                        prod(var(1.into(), type_usize(1)), app(var(1.into(), type_usize(1)), var(2.into(), type_usize(0)))),
                    ),
                ),
            ));

            assert_eq!(term.to_string(), "λ Sort ((max u0 u1) + 1) => λ Type => λ Type 1 => 1 -> 1 2");
            assert_eq!(pretty::Term(term).to_string(), "λ a: Sort ((max u0 u1) + 1) => λ b: Type => λ c: Type 1 => (d: c) -> d c");
        });
    }

    #[test]
    fn display_3() {
        use_arena(|arena| {
            let term = arena.build_term_raw(abs(
                prop(),
                abs(prop(), app(abs(prop(), var(1.into(), prop())), app(var(1.into(), prop()), var(2.into(), prop())))),
            ));
            assert_eq!(term.to_string(), "λ Prop => λ Prop => (λ Prop => 1) (1 2)");
        });
    }

    #[test]
    fn display_axiom() {
        use_arena_with_axioms(|arena| {
            let zero = arena.get_binding("Zero").unwrap();
            let succ = arena.get_binding("Succ").unwrap();
            let two = succ.app(succ.app(zero, arena), arena);

            assert_eq!(two.to_string(), "Succ (Succ Zero)");
        });
    }
}
