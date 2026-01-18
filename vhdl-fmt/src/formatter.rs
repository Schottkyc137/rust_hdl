use std::collections::HashMap;

use crate::{
    config::Config,
    doc_ir::{Doc, boundary::{BoundaryDecision, BreakKind}, resolve::resolve_layout},
};
use vhdl_syntax::{
    syntax::{
        node::{SyntaxNode, SyntaxToken},
        rewrite::{TokenRewrite, TokenRewriteAction, TokenRewriter},
    },
    tokens::{Trivia, TriviaPiece},
};

pub struct Formatter {
    // Configuration options
    config: Config,
}

impl Formatter {
    pub fn new(config: Config) -> Formatter {
        Formatter { config }
    }

    pub fn format(&mut self, node: SyntaxNode) -> SyntaxNode {
        let doc = Doc::from_node(node.clone());
        let layout = resolve_layout(doc, &self.config);
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
    layout: HashMap<usize, BoundaryDecision>,
    previous_trailing_trivia: Option<Trivia>,
    config: Config,
}

impl TokenRewrite for LayoutBasedTokenRewriter {
    fn token(&mut self, token: &SyntaxToken) -> TokenRewriteAction {
        let mut tok = token.clone();

        let mut leading_trivia = Trivia::default();

        if let Some(formatting) = self.layout.get(&token.text_pos()) {
            leading_trivia = formatting.trivia.clone();
            match formatting.break_kind {
                BreakKind::Unset => {}
                BreakKind::Space => leading_trivia.push(TriviaPiece::Spaces(1)),
                BreakKind::Empty => {}
                BreakKind::Newline {
                    blank_lines,
                    indent,
                } => {
                    if blank_lines > 1 {
                        println!("blank_lines > 1")
                    }
                    leading_trivia.push(self.config.newline_style.to_trivia_n(blank_lines + 1));
                    if indent > 0 {
                        leading_trivia.push(self.config.indentation.style.to_trivia(indent));
                    }
                }
            }
        } else {
            debug_assert!(false, "No decision for token {:?}", token);
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
