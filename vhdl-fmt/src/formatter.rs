use std::collections::HashMap;

use crate::{
    config::Config,
    doc_ir::{
        Doc, DocComment,
        boundary::{BoundaryDecision, BreakKind},
        resolve::resolve_layout,
    },
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

fn break_kind_to_trivia(break_kind: BreakKind, trivia: &mut Trivia, config: &Config) {
    match break_kind {
        BreakKind::Unset => {}
        BreakKind::Space => trivia.push(TriviaPiece::Spaces(1)),
        BreakKind::Empty => {}
        BreakKind::Newline {
            blank_lines,
            indent,
        } => {
            trivia.push(config.newline_style.to_trivia_n(blank_lines + 1));
            if indent > 0 {
                trivia.push(config.indentation.style.to_trivia(indent));
            }
        }
    }
}

impl TokenRewrite for LayoutBasedTokenRewriter {
    fn token(&mut self, token: &SyntaxToken) -> TokenRewriteAction {
        let mut tok = token.clone();

        let mut leading_trivia = Trivia::default();

        if let Some(formatting) = self.layout.get(&token.text_pos()) {
            leading_trivia = formatting.trivia.clone();

            for (break_kind, comment) in &formatting.comments {
                break_kind_to_trivia(break_kind.clone(), &mut leading_trivia, &self.config);
                match comment {
                    DocComment::Line(comment) => {
                        leading_trivia.push(TriviaPiece::LineComment(comment.clone()))
                    }
                    DocComment::Block(comment) => {
                        leading_trivia.push(TriviaPiece::BlockComment(comment.clone()))
                    }
                }
            }
            break_kind_to_trivia(formatting.break_kind, &mut leading_trivia, &self.config);
        } else {
            debug_assert!(false, "No decision for token {:?}", token);
        }

        tok = tok.clone_with_leading_trivia(leading_trivia);

        TokenRewriteAction::Replace(tok)
    }
}
