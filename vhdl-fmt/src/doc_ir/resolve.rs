use vhdl_syntax::tokens::TriviaPiece;

use crate::{
    config::Config,
    doc_ir::{
        Doc,
        boundary::{BoundaryDecision, BreakKind},
    },
};
use std::{collections::HashMap, mem::take};

/// Column position before a token, computed from the boundary decision
/// that the formatter will actually emit for that token.
///
/// The formatter emits (in this order):
///   1. Verbatim trivia (only when `break_kind == Unset`)
///   2. For each comment: the comment's preceding `BreakKind`, then the comment text
///   3. The token's own `BreakKind`
///
/// Line comments (`--`) consume the rest of the current line, so the column
/// resets to 0 after them; the subsequent `BreakKind` then provides the
/// fresh indent.
fn col_before_token(decision: &BoundaryDecision, mut column: usize) -> usize {
    if decision.break_kind == BreakKind::Unset {
        // Verbatim trivia: walk the trivia pieces and track the column exactly.
        decision.trivia.byte_len();
        for trivia in &decision.trivia {
            match trivia {
                TriviaPiece::HorizontalTabs(_) => unimplemented!("Column count for tabs"),
                TriviaPiece::VerticalTabs(_)
                | TriviaPiece::CarriageReturns(_)
                | TriviaPiece::CarriageReturnLineFeeds(_)
                | TriviaPiece::LineFeeds(_)
                | TriviaPiece::FormFeeds(_) => column = 0,
                TriviaPiece::LineComment(comment) | TriviaPiece::BlockComment(comment) => {
                    column += comment.byte_len()
                }
                TriviaPiece::Spaces(n) | TriviaPiece::NonBreakingSpaces(n) => column += n,
                TriviaPiece::Unexpected(items) => column += items.len(),
            }
        }
        return column;
    }

    // Formatter-controlled boundary: apply each comment's preceding break, the
    // comment text, and finally the token's own break.
    for (break_kind, comment) in &decision.comments {
        column = col_after_break(column, break_kind);
        column += comment.byte_len();
    }
    col_after_break(column, &decision.break_kind)
}

/// Apply a single `BreakKind` to the current column and return the new column.
fn col_after_break(column: usize, break_kind: &BreakKind) -> usize {
    match break_kind {
        BreakKind::Unset | BreakKind::Empty => column,
        BreakKind::Spaces(n) => column + n,
        BreakKind::Newline { indent, .. } => *indent,
    }
}

impl ResolveState {
    pub fn new() -> ResolveState {
        ResolveState {
            plan: HashMap::new(),
            pending: BoundaryDecision::default(),
            indent: 0,
            column: 0,
            blank_lines_hint: 0,
            current_line_start: None,
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
    /// Accumulated user blank-line count hint
    blank_lines_hint: usize,
    /// Text position of the first token of the current line.
    current_line_start: Option<usize>,
}

fn resolve_layout_recursive(doc: Doc, config: &Config, state: &mut ResolveState, flat: bool) {
    match doc {
        Doc::Token(syntax_token) => {
            let mut boundary_decision = take(&mut state.pending);
            if let BreakKind::Newline { blank_lines, .. } = &mut boundary_decision.break_kind {
                state.current_line_start = Some(syntax_token.text_pos());
                if state.blank_lines_hint != 0 {
                    *blank_lines += state.blank_lines_hint;
                }
            }
            state.blank_lines_hint = 0;
            state.column = col_before_token(&boundary_decision, state.column);
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
            // HardBreak is the highest-priority break: it upgrades any pending
            // break to Newline. If Newline is already pending the HardBreak is a
            // no-op — blank_lines_hint is intentionally left intact so the Token
            // handler can still apply it to the existing Newline boundary.
            if !matches!(state.pending.break_kind, BreakKind::Newline { .. }) {
                state.pending.break_kind = BreakKind::Newline {
                    blank_lines: state.blank_lines_hint,
                    indent: state.indent,
                };
                state.blank_lines_hint = 0;
            }
        }
        Doc::Indent(docs) => {
            state.indent += config.indentation.width;
            for doc in docs {
                resolve_layout_recursive(doc, config, state, flat);
            }
            state.indent -= config.indentation.width;
        }
        Doc::SoftBreak { flat_spaces } => {
            // SoftBreak resolves to Spaces(flat_spaces) when flat, Newline when broken.
            // It never overrides a pending Newline — the structural HardBreak for
            // that boundary is now emitted after any lifted comment trivia in
            // `from_node`, so a SoftBreak that precedes a structural boundary will
            // always arrive before the HardBreak and will not compete with it.
            if !matches!(state.pending.break_kind, BreakKind::Newline { .. }) {
                state.pending.break_kind = if flat {
                    BreakKind::Spaces(flat_spaces)
                } else {
                    BreakKind::Newline {
                        blank_lines: state.blank_lines_hint,
                        indent: state.indent,
                    }
                };
                state.blank_lines_hint = 0;
            }
        }
        Doc::Group(docs) => {
            let layout_as_flat = match docs.flat_width() {
                None => false,
                Some(w) => state.column + w <= config.max_line_length,
            };
            for doc in docs {
                resolve_layout_recursive(doc, config, state, layout_as_flat);
            }
        }
        Doc::Concat(docs) => {
            for doc in docs {
                resolve_layout_recursive(doc, config, state, flat);
            }
        }
        Doc::Comment(comment) => {
            let break_kind = take(&mut state.pending.break_kind);
            state.column = col_after_break(state.column, &break_kind);
            state.column += comment.byte_len();
            state.pending.comments.push((break_kind, comment));
        }
        Doc::TrailingComment(comment) => {
            let break_kind = take(&mut state.pending.break_kind);
            state.column = col_after_break(state.column, &break_kind);
            state.column += comment.byte_len();
            if flat {
                // Flat layout: keep inline just like a regular Comment.
                state.pending.comments.push((break_kind, comment));
            } else {
                // Broken layout: hoist the comment to appear before the
                // current statement by prepending it to the boundary decision
                // of that statement's first token.
                let hoisted = if let Some(stmt_pos) = state.current_line_start {
                    if let Some(decision) = state.plan.get_mut(&stmt_pos) {
                        let indent = match decision.break_kind {
                            BreakKind::Newline { indent, .. } => indent,
                            _ => 0,
                        };
                        // Insert at the front so that the trailing comment
                        // precedes any leading comments already on that token.
                        decision.comments.push((
                            BreakKind::Newline {
                                blank_lines: 0,
                                indent,
                            },
                            comment.clone(),
                        ));
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                if !hoisted {
                    // Fallback: treat like a regular comment.
                    state.pending.comments.push((break_kind, comment));
                }
            }
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
        Doc::Spaces(n) => {
            // Do not override newlines with space
            if matches!(
                state.pending.break_kind,
                BreakKind::Empty | BreakKind::Unset
            ) {
                state.pending.break_kind = BreakKind::Spaces(n);
                state.blank_lines_hint = 0;
            }
            // Existing Newline: don't override; keep blank_lines_hint for the newline
        }
        Doc::AlignedSpace(n) => {
            // AlignedSpace never overrides a pending Newline.
            if !matches!(state.pending.break_kind, BreakKind::Newline { .. }) {
                state.pending.break_kind = if flat {
                    BreakKind::Spaces(1)
                } else {
                    BreakKind::Spaces(n)
                };
            }
        }
        Doc::BlankLines(n) => {
            let hint = match config.blank_lines {
                crate::config::UserBlankLinePolicy::Preserve => n,
            };
            state.blank_lines_hint += hint;
        }
    }
}
