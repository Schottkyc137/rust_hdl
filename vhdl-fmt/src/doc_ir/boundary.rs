//! Boundary decisions
use vhdl_syntax::tokens::Trivia;

use crate::doc_ir::DocComment;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum RuleSep {
    /// No explicit separator.
    /// This is currently for debug purposes and simply
    /// instructs the formatter to keep all trivia.
    #[default]
    Unset,
    /// Exactly `n` spaces before a token
    Spaces(usize),
    /// Add a newline with blank lines and given indent
    Newline { blank_lines: usize, indent: usize },
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
/// Boundary decisions that always apply _before_ a token.
pub struct BoundaryDecision {
    /// How to separate this token from the last one.
    pub rule_sep: RuleSep,
    /// Any verbatim trivia (comments, whitespaces, unknown trivia)
    /// that should be printed before this token
    pub trivia: Trivia,
    pub comments: Vec<(RuleSep, DocComment)>,
}
