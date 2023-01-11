//! Errors that can be yielded by the parser.

use derive_more::Display;
use pest::error::LineColLocation;
use elaboration::location::Location;

use crate::command::parse::Rule;

/// The type representing [parser errors](Kind) with associated [`Location`].
#[derive(Clone, Debug, Display, PartialEq, Eq)]
#[display(fmt = "{kind}")]
pub struct Error {
    /// The kind of form error that occurred.
    pub kind: Kind,

    /// The trace.
    pub location: Location,
}

/// The type of errors that can occur in the parser.
#[non_exhaustive]
#[derive(Clone, Debug, Display, Eq, PartialEq)]
pub enum Kind {
    /// Parsing error (redirection from Pest)
    CannotParse(String),
}

impl From<pest::error::Error<Rule>> for Error {
    #[inline]
    fn from(err: pest::error::Error<Rule>) -> Self {
        // renaming error messages
        let err = err.renamed_rules(|rule| match *rule {
            Rule::string | Rule::Var => "variable".to_owned(),
            Rule::number => "number".to_owned(),
            Rule::Define => "def var := term".to_owned(),
            Rule::Declaration => "def decl.{ vars, ... } := term".to_owned(),
            Rule::DeclarationCheckType => "def decl.{ vars, ... } : term := term".to_owned(),
            Rule::CheckType => "check term : term".to_owned(),
            Rule::GetType => "check term".to_owned(),
            Rule::DefineCheckType => "def var : term := term".to_owned(),
            Rule::Abs => "abstraction".to_owned(),
            Rule::dProd => "dependent product".to_owned(),
            Rule::Prod => "product".to_owned(),
            Rule::App => "application".to_owned(),
            Rule::Prop => "Prop".to_owned(),
            Rule::Type => "Type".to_owned(),
            Rule::Sort => "Sort".to_owned(),
            Rule::Eval => "eval term".to_owned(),
            Rule::filename => "path_to_file".to_owned(),
            Rule::ImportFile => "import path_to_file".to_owned(),
            Rule::Search => "search var".to_owned(),
            Rule::Max => "max".to_owned(),
            Rule::Plus => "plus".to_owned(),
            Rule::IMax => "imax".to_owned(),
            Rule::arg_univ => "universe argument".to_owned(),
            Rule::univ_decl => "universe declaration".to_owned(),
            _ => {
                unreachable!("low level rules cannot appear in error messages")
            },
        });

        // extracting the location from the pest output
        let loc = match err.line_col {
            LineColLocation::Pos((x, y)) => {
                let mut right = y;
                let mut left = 1;
                let chars = err.line().chars();
                let mut i = 0;

                for c in chars {
                    i += 1;
                    if char::is_whitespace(c) {
                        if i < y {
                            left = i + 1;
                        } else {
                            break;
                        }
                    } else {
                        right = i;
                    }
                }

                if i < y {
                    left = y;
                    right = y;
                }

                Location::new((x, left), (x, 1 + right))
            },

            LineColLocation::Span(start, (end_l, end_c)) => Location::new(start, (end_l, 1 + end_c)),
        };

        // extracting the message from the pest output
        let message = err.to_string();
        let mut chars = message.lines().next_back().unwrap_or_else(|| unreachable!()).chars();

        (0_i32..4_i32).for_each(|_| {
            chars.next();
        });

        Self {
            kind: Kind::CannotParse(chars.as_str().to_owned()),
            location: loc,
        }
    }
}

/// The type of results yielded by the parser.
pub type Result<T> = core::result::Result<T, Error>;
