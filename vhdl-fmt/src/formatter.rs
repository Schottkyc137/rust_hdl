use std::collections::HashMap;

use vhdl_syntax::{
    syntax::{
        node::{SyntaxNode, SyntaxToken},
        rewrite::{TokenRewrite, TokenRewriteAction, TokenRewriter},
    },
    tokens::{Trivia, TriviaPiece},
};

use crate::{
    config::{Config, Indentation, NewlineStyle},
    doc_ir::{BoundaryDecision, Doc, TokenFormatting},
};

pub struct Formatter {
    // Configuration options
    config: Config,
}

/// Trims trailing whitespace from the provided trivia.
fn trim_trailing_ws(line: &[TriviaPiece]) -> &[TriviaPiece] {
    if let Some(pos) = line.iter().rposition(|t| !t.is_space_or_tab()) {
        &line[..=pos]
    } else {
        line
    }
}

/// Trims leading whitespace from the provided trivia.
fn trim_leading_ws(line: &[TriviaPiece]) -> &[TriviaPiece] {
    if let Some(pos) = line.iter().position(|t| !t.is_space_or_tab()) {
        &line[pos..]
    } else {
        line
    }
}

fn emit_line(
    out: &mut Trivia,
    line: &[TriviaPiece],
    newline: &TriviaPiece,
    indent: &Indentation,
    indent_level: usize,
) {
    if indent_level > 0 {
        out.push(indent.style.to_trivia(indent_level));
    }
    out.extend(line.iter().cloned());
    out.push(newline.clone());
}

fn normalize_line(line: &[TriviaPiece], is_first: bool) -> &[TriviaPiece] {
    let line = trim_trailing_ws(line);

    if is_first {
        line
    } else {
        trim_leading_ws(line)
    }
}

/// Normalizes trivia:
///
/// - No whitespace after newlines
fn normalize_trivia(trivia: &Trivia, indent: &Indentation, indent_level: usize) -> Trivia {
    let mut out = Trivia::default();
    let mut first_newline = true;

    for chunk in trivia.split_inclusive(TriviaPiece::is_newline) {
        if let Some(newline) = chunk.last().filter(|piece| piece.is_newline()) {
            let content = &chunk[..chunk.len() - 1];
            let normalized = normalize_line(content, first_newline);
            first_newline = false;

            emit_line(&mut out, normalized, newline, indent, indent_level);
        } else {
            if !(!first_newline && chunk.iter().all(TriviaPiece::is_space_or_tab)) {
                out.extend(chunk.iter().cloned());
            }
        }
    }

    out
}

fn ensure_newlines(trivia: &mut Trivia, n: usize, newline_style: NewlineStyle) {
    let count_of_newlines: usize = trivia
        .iter()
        .filter_map(|piece| match piece {
            TriviaPiece::CarriageReturnLineFeeds(n) => Some(n),
            TriviaPiece::LineFeeds(n) => Some(n),
            TriviaPiece::CarriageReturns(n) => Some(n),
            TriviaPiece::FormFeeds(n) => Some(n),
            TriviaPiece::VerticalTabs(n) => Some(n),
            _ => None,
        })
        .copied()
        .sum();
    if count_of_newlines < n {
        trivia.push(newline_style.to_trivia_n(n - count_of_newlines));
    }
}

impl Formatter {
    pub fn new(config: Config) -> Formatter {
        Formatter { config }
    }

    pub fn format(&mut self, node: SyntaxNode) -> SyntaxNode {
        let doc = Doc::from_node(node.clone());
        let layout = doc.resolve_layout(&self.config);
        let layout_rewriter = LayoutBasedTokenRewriter {
            layout,
            previous_trailing_trivia: None,
            config: self.config.clone(),
        };
        let mut rewriter = TokenRewriter::new(layout_rewriter);
        rewriter.rewrite(node)
    }
}

struct LayoutBasedTokenRewriter {
    layout: HashMap<usize, TokenFormatting>,
    previous_trailing_trivia: Option<Trivia>,
    config: Config,
}

impl TokenRewrite for LayoutBasedTokenRewriter {
    fn token(&mut self, token: &SyntaxToken) -> TokenRewriteAction {
        let mut tok = token.clone();

        let mut leading_trivia = Trivia::default();

        if let Some(formatting) = self.layout.get(&token.text_pos()) {
            leading_trivia = formatting.leading_comments.clone();
            match formatting.boundary_decision {
                BoundaryDecision::Space => leading_trivia.push(TriviaPiece::Spaces(1)),
                BoundaryDecision::Empty => {},
                BoundaryDecision::Newline {
                    blank_lines,
                    indent,
                } => {
                    leading_trivia.push(self.config.newline_style.to_trivia_n(blank_lines + 1));
                    if indent > 0 {
                        leading_trivia.push(self.config.indentation.style.to_trivia(indent));
                    }
                }
            }
        }

        // Store the previous trivia and reset trailing trivia of the token.
        // This means that for formatting, we only have to deal with trailing trivia.
        // TODO: Improve this by introducing an EOF token.
        if !tok.next_token().is_none() {
            self.previous_trailing_trivia = Some(tok.trailing_trivia());
            tok = tok.clone_with_trivia(leading_trivia, Trivia::default())
        } else {
            tok = tok.clone_with_leading_trivia(leading_trivia)
        }

        TokenRewriteAction::Replace(tok)
    }
}
