use vhdl_syntax::tokens::TriviaPiece;

use crate::{
    config::Config,
    doc_ir::{
        Doc,
        boundary::{BoundaryDecision, BreakKind},
    },
};
use std::{collections::HashMap, mem::take};

impl ResolveState {
    pub fn new() -> ResolveState {
        ResolveState {
            plan: HashMap::new(),
            pending: BoundaryDecision::default(),
            indent: 0,
            column: 0,
        }
    }
}

pub fn resolve_layout(doc: Doc, config: &Config) -> LayoutPlan {
    let mut state = ResolveState::new();
    resolve_layout_recursive(doc, config, &mut state, true);
    state.plan
}

pub type LayoutPlan = HashMap<usize, BoundaryDecision>;

struct ResolveState {
    plan: LayoutPlan,
    /// The pending boundary decision
    pending: BoundaryDecision,
    /// The current indent
    indent: usize,
    /// The current column
    column: usize,
}

fn resolve_layout_recursive(doc: Doc, config: &Config, state: &mut ResolveState, flat: bool) {
    match doc {
        Doc::Token(syntax_token) => {
            let boundary_decision = take(&mut state.pending);
            for trivia in &boundary_decision.trivia {
                match trivia {
                    TriviaPiece::HorizontalTabs(_) => unimplemented!("Column count for tabs"),
                    TriviaPiece::VerticalTabs(_)
                    | TriviaPiece::CarriageReturns(_)
                    | TriviaPiece::CarriageReturnLineFeeds(_)
                    | TriviaPiece::LineFeeds(_)
                    | TriviaPiece::FormFeeds(_) => state.column = 0,
                    TriviaPiece::LineComment(comment) | TriviaPiece::BlockComment(comment) => {
                        state.column += comment.byte_len()
                    }
                    TriviaPiece::Spaces(n) => state.column += n,
                    TriviaPiece::NonBreakingSpaces(n) => state.column += n,
                    TriviaPiece::Unexpected(items) => state.column += items.len(),
                }
            }
            state
                .plan
                .insert(syntax_token.text_pos(), boundary_decision);

            state.column += syntax_token.text().len();
        }
        Doc::HardBreak => {
            assert!(
                state.pending.trivia.is_empty(),
                "Invariant: trivia before hard break"
            );
            // overwrite any previoud breaks
            state.pending.break_kind = BreakKind::Newline {
                blank_lines: 0,
                indent: state.indent,
            };
        }
        Doc::Indent(doc) => {
            state.indent += config.indentation.width;
            resolve_layout_recursive(*doc, config, state, flat);
            state.indent -= config.indentation.width;
        }
        Doc::SoftBreak => {
            state.pending.break_kind = if flat {
                BreakKind::Space
            } else {
                BreakKind::Newline {
                    indent: state.indent,
                    blank_lines: 0,
                }
            };
        }
        Doc::Group(doc) => {
            let layout_as_flat = if let Some(flat_width) = doc.flat_width() {
                flat && state.column + flat_width <= config.max_line_length
            } else {
                false
            };
            resolve_layout_recursive(*doc, config, state, layout_as_flat);
        }
        Doc::Concat(docs) => {
            for doc in docs {
                resolve_layout_recursive(doc, config, state, flat);
            }
        }
        Doc::Comment(comment) => {
            let break_kind = take(&mut state.pending.break_kind);
            state.column += comment.byte_len();
            state.pending.comments.push((break_kind, comment));
        }
        // BIG TODO: Tokens are currently handled in a very mediocre way.
        // This is because tokens are treated differently from comments - comments are trivia
        // and always part of tokens, but here it would be more sensible to handle them
        // closer to tokens.
        Doc::Trivia(trivia) => {
            if state.pending.break_kind == BreakKind::Unset {
                state.pending.trivia = trivia
            }
        }
        Doc::Space => {
            // Do not override newlines with space
            if matches!(
                state.pending.break_kind,
                BreakKind::Empty | BreakKind::Unset
            ) {
                state.pending.break_kind = BreakKind::Space;
            }
        }
    }
}
