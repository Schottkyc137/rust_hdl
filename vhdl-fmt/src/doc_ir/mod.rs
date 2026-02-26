pub(crate) mod boundary;
mod builder;
pub(crate) mod resolve;

use crate::align::AlignmentMap;
use crate::doc_ir::builder::DocBuilder;
use std::{fmt::Debug, vec};
use vhdl_syntax::{
    syntax::{
        NodeKind,
        node::{SyntaxElement, SyntaxNode, SyntaxToken},
        visitor::{PreorderWithTokens, WalkEvent},
    },
    tokens::{Trivia, TriviaPiece, trivia_piece::Comment},
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

#[derive(Clone)]
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
#[allow(dead_code)]
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
    /// Optional break: space if fits, newline + indent otherwise
    SoftBreak,
    /// A forced space
    Space,
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
            Doc::SoftBreak => Some(1),
            Doc::Space => Some(1),
            Doc::AlignedSpace(_) => Some(1),
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
            Self::SoftBreak => write!(f, "SoftBreak"),
            Self::HardBreak => write!(f, "HardBreak"),
            Self::Indent(arg0) => f.debug_tuple("Indent").field(&arg0.0).finish(),
            Self::Concat(arg0) => f.debug_tuple("Concat").field(&arg0.0).finish(),
            Self::Group(arg0) => f.debug_tuple("Group").field(&arg0.0).finish(),
            Self::Trivia(arg0) => f.debug_tuple("Trivia").field(&arg0.to_string()).finish(),
            Self::Space => write!(f, "Space"),
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

/// Group of nodes that should be indented
fn indents(node: &SyntaxNode) -> bool {
    use vhdl_syntax::syntax::NodeKind as Nk;
    match node.kind() {
        Nk::ContextClause
            if node
                .parent()
                .is_some_and(|par| par.kind() == Nk::ContextDeclaration) =>
        {
            true
        }
        Nk::Declarations
        | Nk::ConcurrentStatements
        | Nk::SequentialStatements
        | Nk::BlockConfigurationItems
        | Nk::BlockHeader
        | Nk::GenerateStatementBody
        | Nk::CaseGenerateAlternative
        | Nk::CaseStatementAlternative
        | Nk::ComponentConfigurationItems
        | Nk::ComponentDeclarationItems
        | Nk::ComponentInstantiationItems
        | Nk::CompoundConfigurationSpecificationItems
        | Nk::ConfigurationDeclarationItems
        | Nk::EntityHeader
        | Nk::PackageHeader
        | Nk::UnitDeclarations
        | Nk::RecordElementDeclarations
        | Nk::InterfaceList => true,
        _ => false,
    }
}

/// Group of nodes that have optionally flat layout
fn groups(node: &SyntaxNode) -> bool {
    matches!(
        node.kind(),
        NodeKind::InterfaceList
            | NodeKind::GenericClause
            | NodeKind::PortClause
            | NodeKind::ParenthesizedInterfaceList
            | NodeKind::RecordElementDeclarations
    )
}

fn break_kind_before(node: NodeKind) -> Option<Doc> {
    use vhdl_syntax::syntax::NodeKind as Nk;
    match node {
        Nk::AliasDeclaration
        | Nk::DeclarationStatementSeparator
        | Nk::SemiColonTerminatedBindingIndication
        | Nk::UseClause
        | Nk::SubprogramDeclaration
        | Nk::SubprogramBody
        | Nk::SubprogramInstantiationDeclaration
        | Nk::PackageDeclaration
        | Nk::PackageBody
        | Nk::PackageInstantiationDeclaration
        | Nk::FullTypeDeclaration
        | Nk::IncompleteTypeDeclaration
        | Nk::SubtypeDeclaration
        | Nk::ConstantDeclaration
        | Nk::SignalDeclaration
        | Nk::VariableDeclaration
        | Nk::SharedVariableDeclaration
        | Nk::FileDeclaration
        | Nk::ComponentDeclaration
        | Nk::AttributeDeclaration
        | Nk::GroupTemplateDeclaration
        | Nk::GroupDeclaration
        | Nk::AttributeSpecification
        | Nk::SimpleConfigurationSpecification
        | Nk::CompoundConfigurationSpecification
        | Nk::DisconnectionSpecification
        | Nk::PslPropertyDeclaration
        | Nk::PslSequenceDeclaration
        | Nk::PslClockDeclaration
        | Nk::GenericClause
        | Nk::PortClause
        | Nk::GenericMapAspect
        | Nk::PortMapAspect
        | Nk::BlockHeader
        | Nk::GenerateStatementBody
        | Nk::CaseGenerateAlternative
        | Nk::CaseStatementAlternative
        | Nk::SemiColonTerminatedVerificationUnitBindingIndication
        | Nk::BlockConfiguration
        | Nk::BlockStatement
        | Nk::ProcessStatement
        | Nk::ConcurrentAssertionStatement
        | Nk::ComponentInstantiationStatement
        | Nk::ConcurrentSelectedSignalAssignment
        | Nk::ConcurrentConditionalSignalAssignment
        | Nk::ConcurrentSimpleSignalAssignment
        | Nk::ConcurrentProcedureCallOrComponentInstantiationStatement
        | Nk::ForGenerateStatement
        | Nk::IfGenerateElsif
        | Nk::IfGenerateElse
        | Nk::CaseGenerateStatement
        | Nk::PslDirective
        | Nk::WaitStatement
        | Nk::AssertionStatement
        | Nk::ReportStatement
        | Nk::ProcedureCallStatement
        | Nk::SimpleVariableAssignment
        | Nk::ConditionalVariableAssignment
        | Nk::SelectedVariableAssignment
        | Nk::IfStatement
        | Nk::IfStatementElsif
        | Nk::IfStatementElse
        | Nk::CaseStatement
        | Nk::LoopStatement
        | Nk::NextStatement
        | Nk::ExitStatement
        | Nk::ReturnStatement
        | Nk::NullStatement
        | Nk::PackageBodyDeclaration
        | Nk::BlockPreamble
        | Nk::PackagePreamble
        | Nk::IfStatementPreamble
        | Nk::PackageBodyPreamble
        | Nk::ArchitecturePreamble
        | Nk::CaseStatementPreamble
        | Nk::LoopStatementPreamble
        | Nk::SubprogramBodyPreamble
        | Nk::ProcessStatementPreamble
        | Nk::EntityDeclarationPreamble
        | Nk::ProtectedTypeBodyPreamble
        | Nk::BlockConfigurationPreamble
        | Nk::ContextDeclarationPreamble
        | Nk::IfGenerateStatementPreamble
        | Nk::ComponentDeclarationPreamble
        | Nk::ForGenerateStatementPreamble
        | Nk::RecordTypeDefinitionPreamble
        | Nk::CaseGenerateStatementPreamble
        | Nk::ComponentConfigurationPreamble
        | Nk::ConfigurationDeclarationPreamble
        | Nk::ProtectedTypeDeclarationPreamble
        | Nk::BlockEpilogue
        | Nk::PackageEpilogue
        | Nk::IfStatementEpilogue
        | Nk::PackageBodyEpilogue
        | Nk::ArchitectureEpilogue
        | Nk::CaseStatementEpilogue
        | Nk::LoopStatementEpilogue
        | Nk::SubprogramBodyEpilogue
        | Nk::ProcessStatementEpilogue
        | Nk::EntityDeclarationEpilogue
        | Nk::ProtectedTypeBodyEpilogue
        | Nk::BlockConfigurationEpilogue
        | Nk::ContextDeclarationEpilogue
        | Nk::IfGenerateStatementEpilogue
        | Nk::ComponentDeclarationEpilogue
        | Nk::ForGenerateStatementEpilogue
        | Nk::RecordTypeDefinitionEpilogue
        | Nk::CaseGenerateStatementEpilogue
        | Nk::GenerateStatementBodyEpilogue
        | Nk::ComponentConfigurationEpilogue
        | Nk::PhysicalTypeDefinitionEpilogue
        | Nk::ConfigurationDeclarationEpilogue
        | Nk::ProtectedTypeDeclarationEpilogue
        | Nk::PrimaryUnitDeclaration
        | Nk::SecondaryUnitDeclaration
        | Nk::ElementDeclaration
        | Nk::SimpleWaveformAssignment
        | Nk::SimpleForceAssignment
        | Nk::SimpleReleaseAssignment => Some(Doc::HardBreak),
        Nk::InterfaceList
        | Nk::InterfaceConstantDeclaration
        | Nk::InterfaceSignalDeclaration
        | Nk::InterfaceVariableDeclaration
        | Nk::InterfaceFileDeclaration
        | Nk::InterfaceIncompleteTypeDeclaration
        | Nk::InterfaceSubprogramDeclaration
        | Nk::InterfacePackageDeclaration
        | Nk::PortClauseEpilogue
        | Nk::GenericClauseEpilogue => Some(Doc::SoftBreak),
        _ => None,
    }
}

impl Doc {
    pub fn from_node(node: SyntaxNode, alignment: &AlignmentMap) -> Doc {
        let mut builder = DocBuilder::new();
        let preorder = PreorderWithTokens::new(node);
        let mut pending_break = None;

        for event in preorder {
            match event {
                WalkEvent::Enter(SyntaxElement::Node(node)) => {
                    if let Some(doc) = break_kind_before(node.kind()) {
                        // No breaks on the first token
                        if node
                            .first_token()
                            .and_then(|tok| tok.prev_token())
                            .is_some()
                        {
                            pending_break = Some(doc);
                        }
                    }

                    if groups(&node) {
                        builder.start_group();
                    } else {
                        builder.start_concat();
                    }

                    if indents(&node) {
                        builder.start_indent();
                    }
                }
                WalkEvent::Enter(SyntaxElement::Token(token)) => {
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
                                            builder.soft_break();
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
                                            builder.soft_break();
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
                            pending_break = Some(Doc::HardBreak);
                        }
                    } else {
                        builder.blank_lines(count_user_blank_lines(token.leading_trivia()));
                    }

                    let has_break = pending_break.is_some();
                    // Step 2: structural break (after comments, before token).
                    if let Some(pending) = pending_break.take() {
                        builder.push(pending);
                    }

                    // Step 3: alignment space — emitted *before* trivia so that the resolver
                    // sets break_kind first; the subsequent trivia emission is then a no-op
                    // (resolver ignores trivia when break_kind != Unset). For structural break
                    // positions the Newline guard prevents AlignedSpace from overriding Newline.
                    if let Some(alignment) = alignment.get(&token) {
                        builder.aligned_spaces(alignment);
                    }

                    // Step 4: verbatim trivia for non-structural positions. Emitted after
                    // aligned_spaces so it is silently ignored whenever aligned_spaces (or the
                    // structural HardBreak) already owns the break_kind slot.
                    if !has_break && !token.leading_trivia().contains_comments() {
                        builder.trivia(token.leading_trivia().clone());
                    }

                    // Step 5: token.
                    builder.token(token.clone());
                }
                WalkEvent::Leave(SyntaxElement::Token(_)) => {}
                WalkEvent::Leave(SyntaxElement::Node(node)) => {
                    if groups(&node) {
                        builder.end_group();
                    } else {
                        builder.end_concat();
                    }

                    if indents(&node) {
                        builder.end_indent();
                    }
                }
            }
        }
        builder.build()
    }
}
