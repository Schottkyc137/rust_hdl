pub(crate) mod boundary;
mod builder;
pub(crate) mod resolve;

use crate::doc_ir::builder::DocBuilder;
use crate::props::node_prop::{BreakKind, ChildLayout, SelfLayout};
use crate::{align::AlignmentMap, config::Config, props::node_kind_prop};
use std::mem::take;
use std::{fmt::Debug, vec};
use vhdl_syntax::{
    syntax::{
        node::{SyntaxElement, SyntaxNode, SyntaxToken},
        visitor::{PreorderWithTokens, WalkEvent},
    },
    tokens::{TokenKind, Trivia, TriviaPiece, trivia_piece::Comment},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DocComment {
    Line(Comment),
    Block(Comment),
}

impl DocComment {
    pub fn byte_len(&self) -> usize {
        match self {
            DocComment::Line(comment) | DocComment::Block(comment) => comment.byte_len(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Docs(pub Vec<Doc>);

impl Docs {
    pub fn flat_width(&self) -> Option<usize> {
        self.0.iter().map(|doc| doc.flat_width()).sum()
    }
}

impl IntoIterator for Docs {
    type Item = Doc;

    type IntoIter = vec::IntoIter<Doc>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Clone)]
pub enum Doc {
    /// The basic element of text
    Token(SyntaxToken),
    /// User-supplied trivia
    Trivia(Trivia),
    /// Mandatory newline
    HardBreak,
    /// Indent the following docs
    Indent(Docs),
    /// Sequence of docs. Rendered without any special consideration.
    Concat(Docs),
    /// Group: try to keep flat if it fits max width
    Group(Docs),
    /// A comment
    Comment(DocComment),
    /// Optional break: 'n' spaces if fits, newline + indent otherwise
    SoftBreak { flat_spaces: usize },
    /// forced spaces
    Spaces(usize),
    /// Alignment space: `n` spaces in broken layout, 1 space in flat layout.
    AlignedSpace(usize),
    /// User-supplied blank lines. Only contributes to `BreakKind::Newline.blank_lines`
    /// when the resolver is emitting a newline; ignored otherwise.
    BlankLines(usize),
    /// A comment that appeared on the same source line as the preceding token
    /// (i.e., preceded only by whitespace, not by a newline, in the token's
    /// leading trivia).
    ///
    /// - **Flat layout**: emitted inline after a single space, same as `Comment`.
    /// - **Broken layout**: hoisted to appear *before* the current statement
    ///   (i.e., prepended to the boundary of the first token of the statement
    ///   that owns the comment's `StatementStart` sentinel).
    TrailingComment(DocComment),
}

impl Doc {
    pub fn flat_width(&self) -> Option<usize> {
        match self {
            Doc::Token(syntax_token) => Some(syntax_token.text().len()),
            Doc::HardBreak => None,
            Doc::Indent(doc) => doc.flat_width(),
            Doc::Concat(docs) => docs.flat_width(),
            Doc::Group(docs) => docs.flat_width(),
            // Count soft break as space with flat layouting
            Doc::SoftBreak { flat_spaces } => Some(*flat_spaces),
            Doc::Spaces(n) => Some(*n),
            Doc::AlignedSpace(_) => Some(0),
            Doc::Trivia(trivia) => {
                if trivia.has_newline() {
                    None
                } else {
                    Some(trivia.byte_len())
                }
            }
            Doc::Comment(comment) => Some(comment.byte_len()),
            // Blank lines only contribute when not laying out flat.
            // In a flat layout, they are omitted.
            Doc::BlankLines(_) => Some(0),
            // Trailing comment is inline (space + comment) in flat layout.
            Doc::TrailingComment(comment) => Some(1 + comment.byte_len()),
        }
    }
}

impl Debug for Doc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(arg0) => f.debug_tuple("Token").field(&arg0.text()).finish(),
            Self::SoftBreak { flat_spaces } => f
                .debug_struct("SoftBreak")
                .field("flat_spaces", flat_spaces)
                .finish(),
            Self::HardBreak => write!(f, "HardBreak"),
            Self::Indent(arg0) => f.debug_tuple("Indent").field(&arg0.0).finish(),
            Self::Concat(arg0) => f.debug_tuple("Concat").field(&arg0.0).finish(),
            Self::Group(arg0) => f.debug_tuple("Group").field(&arg0.0).finish(),
            Self::Trivia(arg0) => f.debug_tuple("Trivia").field(&arg0.to_string()).finish(),
            Self::Spaces(arg0) => f.debug_tuple("Spaces").field(arg0).finish(),
            Self::AlignedSpace(n) => write!(f, "AlignedSpace({n})"),
            Self::Comment(arg0) => f.debug_tuple("Comment").field(arg0).finish(),
            Self::BlankLines(n) => write!(f, "BlankLines({n})"),
            Self::TrailingComment(arg0) => f.debug_tuple("TrailingComment").field(arg0).finish(),
        }
    }
}

/// Tracks whitespace/newline separator state while scanning comment trivia.
#[derive(Default)]
struct PendingSep {
    /// True if at least one linefeed-class piece was seen.
    hard: bool,
    /// Total count of linefeed characters accumulated.
    total_linefeeds: usize,
}

impl PendingSep {
    /// Blank lines = total linefeeds minus the single mandatory newline.
    fn blank_lines(&self) -> usize {
        self.total_linefeeds.saturating_sub(1)
    }

    fn add_linefeeds(&mut self, n: usize) {
        self.hard = true;
        self.total_linefeeds += n;
    }
}

/// Count user-supplied blank lines in `trivia`.
/// Blank lines = (total linefeed characters) − 1 (the mandatory single newline).
fn count_user_blank_lines(trivia: &Trivia) -> usize {
    trivia.count_newlines().saturating_sub(1)
}

fn render_self_layout(layout: &SelfLayout) -> BreakKind {
    match layout {
        SelfLayout::Default => BreakKind::Unset,
        SelfLayout::OwnLine => BreakKind::Hard,
        SelfLayout::Joined => BreakKind::Void,
        SelfLayout::SpaceJoined => BreakKind::Spaces(1),
    }
}

fn render_child_node_sep(layout: &ChildLayout, is_first: bool) -> BreakKind {
    match layout {
        ChildLayout::Default => BreakKind::Unset,
        ChildLayout::SpaceSeparated => {
            if is_first {
                BreakKind::Void
            } else {
                BreakKind::Spaces(1)
            }
        }
        ChildLayout::ItemList => {
            if is_first {
                BreakKind::Void
            } else {
                BreakKind::Soft { flat_spaces: 1 }
            }
        }
        ChildLayout::Parenthesized => BreakKind::Soft { flat_spaces: 0 },
    }
}

fn render_child_token_sep(layout: &ChildLayout, is_first: bool) -> BreakKind {
    match layout {
        ChildLayout::Default => BreakKind::Unset,
        ChildLayout::SpaceSeparated => {
            if is_first {
                BreakKind::Void
            } else {
                BreakKind::Spaces(1)
            }
        }
        ChildLayout::ItemList => BreakKind::Unset,
        ChildLayout::Parenthesized => BreakKind::Unset,
    }
}

/// Returns the separator to emit before a direct child node of its parent.
fn separation_before_child_node(child: &SyntaxNode) -> BreakKind {
    let Some(parent) = child.parent() else {
        return BreakKind::Unset;
    };
    let props = node_kind_prop(parent.kind());
    let is_first = child.prev_sibling().is_none();
    render_child_node_sep(&props.child_layout, is_first)
}

/// Returns the separator to emit before a direct child token of its parent.
fn separation_before_child_token(token: &SyntaxToken, config: &Config) -> BreakKind {
    if matches!(token.kind(), TokenKind::SemiColon | TokenKind::Eof) {
        return BreakKind::Void;
    }
    if token.kind() == TokenKind::Colon {
        return if config.space_before_colon {
            BreakKind::Spaces(1)
        } else {
            BreakKind::Void
        };
    }
    let parent = token.parent();
    let props = node_kind_prop(parent.kind());
    let is_last = token.next_sibling_or_token().is_none();
    if is_last && props.child_layout == ChildLayout::Parenthesized {
        return BreakKind::Soft { flat_spaces: 0 };
    }
    let is_first = token.prev_sibling_or_token().is_none();
    render_child_token_sep(&props.child_layout, is_first)
}

impl Doc {
    pub fn from_node(node: SyntaxNode, alignment: &AlignmentMap, config: &Config) -> Doc {
        let mut builder = DocBuilder::new();
        let preorder = PreorderWithTokens::new(node);
        let mut pending_break = BreakKind::Unset;

        for event in preorder {
            match event {
                WalkEvent::Enter(SyntaxElement::Node(node)) => {
                    let props = node_kind_prop(node.kind());
                    // Inject space-sep / soft-break before break_kind_before so that
                    // structural HardBreak can override it.
                    let sep_break = separation_before_child_node(&node);
                    if sep_break.priority() > pending_break.priority() {
                        pending_break = sep_break;
                    }
                    // No breaks on the first token
                    if node
                        .first_token()
                        .and_then(|tok| tok.prev_token())
                        .is_none()
                    {
                        pending_break = BreakKind::Void;
                    } else {
                        let self_break = render_self_layout(&props.self_layout);
                        if self_break.priority() > pending_break.priority() {
                            pending_break = self_break;
                        }
                    }

                    if props.groups {
                        builder.start_group();
                    } else {
                        builder.start_concat();
                    }

                    if props.indents {
                        builder.start_indent();
                    }
                }
                WalkEvent::Enter(SyntaxElement::Token(token)) => {
                    // Step 0: inject space-sep before comment lifting, so that a HardBreak
                    // from comments in Step 1 can still override the injected Spaces(1).
                    let separation = separation_before_child_token(&token, config);
                    if separation.priority() > pending_break.priority() {
                        pending_break = separation;
                    }
                    // Step 1: lift comment trivia (or record blank lines for the plain case).
                    if token.leading_trivia().contains_comments() {
                        let mut last_sep: Option<PendingSep> = None;
                        for triv in token.leading_trivia() {
                            match triv {
                                TriviaPiece::HorizontalTabs(_)
                                | TriviaPiece::Spaces(_)
                                | TriviaPiece::NonBreakingSpaces(_) => {
                                    if last_sep.is_none() {
                                        last_sep = Some(PendingSep::default());
                                    }
                                }
                                TriviaPiece::VerticalTabs(n)
                                | TriviaPiece::CarriageReturnLineFeeds(n)
                                | TriviaPiece::LineFeeds(n)
                                | TriviaPiece::CarriageReturns(n)
                                | TriviaPiece::FormFeeds(n) => {
                                    last_sep.get_or_insert_default().add_linefeeds(*n);
                                }
                                TriviaPiece::BlockComment(comment) => {
                                    if let Some(sep) = last_sep.take() {
                                        if sep.hard {
                                            builder.blank_lines(sep.blank_lines());
                                            builder.hard_break();
                                        } else {
                                            builder.soft_break(1);
                                        }
                                    }
                                    builder.comment(DocComment::Block(comment.clone()));
                                }
                                TriviaPiece::LineComment(comment) => {
                                    if let Some(sep) = last_sep.take() {
                                        if sep.hard {
                                            // Leading line comment: on its own line.
                                            builder.blank_lines(sep.blank_lines());
                                            builder.hard_break();
                                            builder.comment(DocComment::Line(comment.clone()));
                                        } else {
                                            // Trailing line comment: same source line as
                                            // the preceding token.
                                            builder.soft_break(1);
                                            builder.trailing_comment(DocComment::Line(
                                                comment.clone(),
                                            ));
                                        }
                                    } else {
                                        builder.trailing_comment(DocComment::Line(comment.clone()));
                                    }
                                }
                                TriviaPiece::Unexpected(_) => unimplemented!("Unexpected trivia"),
                            }
                        }
                        // The trailing separator after the last comment owns the boundary
                        // (last comment -> token). Blank lines are always preserved.
                        // For structural break positions the structural HardBreak below
                        // handles the mandatory newline; for all other positions we emit it
                        // here as before.
                        // Soft-only separators are dropped — the comment already provides spacing.
                        if let Some(sep) = last_sep.take()
                            && sep.hard
                        {
                            builder.blank_lines(sep.blank_lines());
                            pending_break = BreakKind::Hard;
                        }
                    } else {
                        builder.blank_lines(count_user_blank_lines(token.leading_trivia()));
                    }

                    let mut push_trivia = false;
                    // Step 2: structural break (after comments, before token).
                    match take(&mut pending_break) {
                        BreakKind::Unset => push_trivia = true,
                        BreakKind::Hard => builder.hard_break(),
                        BreakKind::Soft { flat_spaces } => builder.soft_break(flat_spaces),
                        BreakKind::Void => {}
                        BreakKind::Spaces(n) => builder.spaces(n),
                    }

                    // Step 3: alignment space
                    if let Some(alignment) = alignment.get(&token) {
                        builder.aligned_spaces(alignment);
                    }

                    // Step 4: verbatim trivia for non-structural positions. Emitted after
                    // aligned_spaces so it is silently ignored whenever aligned_spaces (or the
                    // structural HardBreak) already owns the break_kind slot.
                    if push_trivia {
                        builder.trivia(token.leading_trivia().clone());
                    }

                    // Step 5: token.
                    builder.token(token.clone());
                }
                WalkEvent::Leave(SyntaxElement::Token(_)) => {}
                WalkEvent::Leave(SyntaxElement::Node(node)) => {
                    let props = node_kind_prop(node.kind());
                    if props.indents {
                        builder.end_indent();
                    }

                    if props.groups {
                        builder.end_group();
                    } else {
                        builder.end_concat();
                    }
                }
            }
        }
        builder.build()
    }
}
