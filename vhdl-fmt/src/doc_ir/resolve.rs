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
            blank_lines_hint: 0,
            last_line_start: None,
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
    last_line_start: Option<usize>,
}

fn resolve_layout_recursive(doc: Doc, config: &Config, state: &mut ResolveState, flat: bool) {
    match doc {
        Doc::Token(syntax_token) => {
            let mut boundary_decision = take(&mut state.pending);
            match &mut boundary_decision.break_kind {
                BreakKind::Newline {
                    blank_lines,
                    indent: _,
                } => {
                    if state.blank_lines_hint != 0 {
                        *blank_lines = *blank_lines + state.blank_lines_hint
                    }
                    state.last_line_start = Some(syntax_token.text_pos());
                }
                _ => {}
            }
            state.blank_lines_hint = 0;
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
        Doc::SoftBreak => {
            // SoftBreak resolves to Spaces(1) when flat, Newline when broken.
            // It never overrides a pending Newline — the structural HardBreak for
            // that boundary is now emitted *after* any lifted comment trivia in
            // `from_node`, so a SoftBreak that precedes a structural boundary will
            // always arrive before the HardBreak and will not compete with it.
            if !matches!(state.pending.break_kind, BreakKind::Newline { .. }) {
                state.pending.break_kind = if flat {
                    BreakKind::Spaces(1)
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
            let layout_as_flat = if let Some(flat_width) = docs.flat_width() {
                flat && state.column + flat_width <= config.max_line_length
            } else {
                false
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
            state.column += comment.byte_len();
            state.pending.comments.push((break_kind, comment));
        }
        Doc::TrailingComment(comment) => {
            let break_kind = take(&mut state.pending.break_kind);
            state.column += comment.byte_len();
            if flat {
                // Flat layout: keep inline just like a regular Comment.
                state.pending.comments.push((break_kind, comment));
            } else {
                // Broken layout: hoist the comment to appear before the
                // current statement by prepending it to the boundary decision
                // of that statement's first token.
                let hoisted = if let Some(stmt_pos) = state.last_line_start {
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
        Doc::Space => {
            // Do not override newlines with space
            if matches!(
                state.pending.break_kind,
                BreakKind::Empty | BreakKind::Unset
            ) {
                state.pending.break_kind = BreakKind::Spaces(1);
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
