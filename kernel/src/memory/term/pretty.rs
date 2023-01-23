//! Pretty-printing of terms.

use core::fmt;

use super::Payload::{Abs, App, Axiom, Decl, Prod, Sort, Var};

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
                write!(f, "(")?;
                fun.pretty_print(f, depth, distance, is_root_closed)?;
                write!(f, ") (")?;
                arg.pretty_print(f, depth, distance, is_root_closed)?;
                write!(f, ")")
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
        }
    }
}
