use vhdl_syntax::{
    syntax::{
        AstNode, ConcurrentStatementSyntax, DeclarationSyntax, NodeKind, SequentialStatementSyntax,
        node::{SyntaxNode, SyntaxToken},
        rewrite::{TokenRewrite, TokenRewriteAction, TokenRewriter},
    },
    tokens::TriviaPiece,
};

use crate::{
    config::Config,
    rule::ALL_RULES,
    state::{RegionSeparator, State},
};

pub struct Formatter {
    // Configuration options
    config: Config,
    // The state of the formater.
    // Deals with indentation levels and pending separators
    // (e.g., newlines and spaces).
    state: State,
}

impl Formatter {
    pub fn new(config: Config) -> Formatter {
        Formatter {
            config,
            state: State::new(),
        }
    }

    fn format_token(&mut self, token: &SyntaxToken) -> SyntaxToken {
        let mut tok = ALL_RULES.iter().fold(token.clone(), |token, rule| {
            if (rule.applies)(&token, &mut self.state) {
                (rule.apply)(&token, &mut self.state, &self.config)
            } else {
                token.clone()
            }
        });

        if let Some(separator) = self.state.get_and_reset_pending_separator() {
            let mut trivia = tok.leading_trivia();
            match separator {
                RegionSeparator::Space => {
                    trivia.push(TriviaPiece::Spaces(1));
                }
                RegionSeparator::Newline => {
                    trivia.push(self.config.newline_style.to_trivia());
                    if self.state.current_indent() != 0 {
                        trivia.push(
                            self.config
                                .indentationn
                                .to_trivia(self.state.current_indent()),
                        );
                    }
                }
            }
            tok = tok.clone_with_leading_trivia(trivia);
        }
        tok
    }

    pub fn format(&mut self, node: SyntaxNode) -> SyntaxNode {
        let mut rewriter = TokenRewriter::new(FormattingTokenRewriter::new(self));
        rewriter.rewrite(node)
    }
}

struct FormattingTokenRewriter<'a> {
    formatter: &'a mut Formatter,
}

impl<'a> FormattingTokenRewriter<'a> {
    pub fn new(formatter: &'a mut Formatter) -> FormattingTokenRewriter<'a> {
        Self { formatter }
    }

    fn set_pending_newline(&mut self) {
        self.set_pending_separator(RegionSeparator::Newline);
    }

    fn set_pending_space(&mut self) {
        self.set_pending_separator(RegionSeparator::Space);
    }

    fn set_pending_separator(&mut self, separator: RegionSeparator) {
        self.formatter.state.set_pending_separator(separator);
    }

    fn indent(&mut self) {
        self.formatter.state.indent()
    }

    fn dedent(&mut self) {
        self.formatter.state.dedent()
    }
}

impl<'a> TokenRewrite for FormattingTokenRewriter<'a> {
    // Add a leading trivia piece before the next token
    fn enter(&mut self, node: &SyntaxNode) {
        match node.kind() {
            NodeKind::ArchitectureEnd => {
                self.set_pending_newline();
            }
            NodeKind::ArchitectureStatementPart | NodeKind::ArchitectureDeclarativePart => {
                self.indent();
            }
            _ => {
                if DeclarationSyntax::can_cast(node)
                    || ConcurrentStatementSyntax::can_cast(node)
                    || SequentialStatementSyntax::can_cast(node)
                {
                    self.set_pending_newline();
                }
            }
        }
    }

    fn token(&mut self, token: &SyntaxToken) -> TokenRewriteAction {
        let new_tok = self.formatter.format_token(token);
        if &new_tok == token {
            TokenRewriteAction::Keep
        } else {
            TokenRewriteAction::Replace(new_tok)
        }
    }

    fn exit(&mut self, node: &SyntaxNode) {
        // Add a leading trivia piece before the next token
        match node.kind() {
            NodeKind::ArchitectureHeader => {
                self.set_pending_newline();
            }
            NodeKind::ArchitectureDeclarativePart => {
                self.dedent();
                self.set_pending_newline();
            }
            NodeKind::ArchitectureStatementPart => {
                self.dedent();
            }
            NodeKind::Name => {
                if node.parent().map(|parent| parent.kind()) == Some(NodeKind::ArchitectureHeader) {
                    self.set_pending_space();
                }
            }
            _ => {}
        }
    }
}
