use vhdl_syntax::{
    syntax::{
        NodeKind,
        node::SyntaxToken,
    },
    tokens::{Keyword, TokenKind, TriviaPiece},
};

use crate::{config::Config, state::State};

pub struct Rule {
    pub applies: fn(&SyntaxToken, &State) -> bool,
    pub apply: fn(&SyntaxToken, &mut State, &Config) -> SyntaxToken,
}

fn single_trailing_whitespace(token: &SyntaxToken) -> SyntaxToken {
    let mut trailing_trivia = token.trailing_trivia().clone();
    trailing_trivia.remove_trailing_whitespaces();
    trailing_trivia.push(TriviaPiece::Spaces(1));

    let leading_trivia = token.leading_trivia().clone_without_leading_whitespaces();
    token.clone_with_trivia(leading_trivia, trailing_trivia)
}

fn newline_after(token: &SyntaxToken) -> SyntaxToken {
    let mut trailing_trivia = token.trailing_trivia().clone();
    trailing_trivia.remove_trailing_whitespaces();
    trailing_trivia.push(TriviaPiece::LineFeeds(1));

    let leading_trivia = token.leading_trivia().clone_without_leading_whitespaces();
    token.clone_with_trivia(leading_trivia, trailing_trivia)
}

fn no_space_before(token: &SyntaxToken) -> SyntaxToken {
    let leading_trivia = token.leading_trivia().clone_without_leading_whitespaces();
    token.clone_with_leading_trivia(leading_trivia)
}

fn no_space_after(token: &SyntaxToken) -> SyntaxToken {
    let mut trailing_trivia = token.trailing_trivia().clone();
    trailing_trivia.remove_trailing_whitespaces();
    token.clone_with_trailing_trivia(trailing_trivia)
}

fn no_spaces(token: &SyntaxToken) -> SyntaxToken {
    let mut trailing_trivia = token.trailing_trivia().clone();
    trailing_trivia.remove_trailing_whitespaces();

    let leading_trivia = token.leading_trivia().clone_without_leading_whitespaces();
    token.clone_with_trivia(leading_trivia, trailing_trivia)
}

// Preamble tokens are separated by a single space
const PREAMBLE_TOKENS: Rule = Rule {
    applies: |tok, _| {
        matches!(
            tok.parent().kind(),
            NodeKind::ArchitecturePreamble | NodeKind::BlockPreamble
        )
    },
    apply: |tok, _, _| {
        if tok.is_last_sibling() {
            no_spaces(tok)
        } else {
            single_trailing_whitespace(tok)
        }
    },
};

// Epilogue tokens are separated by a single space except for the token before the semicolon
const EPILOGUE_TOKENS: Rule = Rule {
    applies: |tok, _| {
        matches!(
            tok.parent().kind(),
            NodeKind::ArchitectureEpilogue | NodeKind::BlockEpilogue
        )
    },
    apply: |tok, _, _| {
        if tok.kind() == TokenKind::SemiColon
            || tok
                .next_token()
                .is_some_and(|tok| tok.kind() == TokenKind::SemiColon)
        {
            no_spaces(tok)
        } else {
            single_trailing_whitespace(tok)
        }
    },
};

const END_OF_FILE: Rule = Rule {
    applies: |tok, _| tok.next_token().is_none(),
    apply: |tok, _, config| {
        if config.newline_at_end {
            newline_after(tok)
        } else {
            no_spaces(tok)
        }
    },
};

const NO_SPACES_AROUND_NODES: Rule = Rule {
    applies: |tok, _| tok.is_first_sibling() || tok.is_last_sibling(),
    apply: |tok, _, _| {
        if tok.is_first_sibling() && tok.is_last_sibling() {
            no_spaces(tok)
        } else if tok.is_first_sibling() {
            no_space_before(tok)
        } else if tok.is_last_sibling() {
            no_space_after(tok)
        } else {
            tok.clone()
        }
    },
};

const NO_SPACE_AROUND_BEGIN: Rule = Rule {
    applies: |tok, _| tok.kind() == TokenKind::Keyword(Keyword::Begin),
    apply: |tok, _, _| no_spaces(tok),
};

pub const ALL_RULES: &[Rule] = &[
    PREAMBLE_TOKENS,
    EPILOGUE_TOKENS,
    NO_SPACES_AROUND_NODES,
    NO_SPACE_AROUND_BEGIN,
    END_OF_FILE,
];
