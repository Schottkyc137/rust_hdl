pub(crate) mod boundary;
mod builder;
pub(crate) mod resolve;

use crate::align::AlignmentMap;
use crate::doc_ir::builder::DocBuilder;
use std::fmt::Debug;
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
#[allow(dead_code)]
pub enum Doc {
    /// The basic element of text
    Token(SyntaxToken),
    /// User-supplied trivia
    Trivia(Trivia),
    /// Mandatory newline
    HardBreak,
    /// Indent the following docs
    Indent(Box<Doc>),
    /// Sequence of docs. Rendered without any special consideration.
    Concat(Vec<Doc>),
    /// Group: try to keep flat if it fits max width
    Group(Box<Doc>),
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
}

impl Doc {
    pub fn flat_width(&self) -> Option<usize> {
        match self {
            Doc::Token(syntax_token) => Some(syntax_token.text().len()),
            Doc::HardBreak => None,
            Doc::Indent(doc) => doc.flat_width(),
            Doc::Concat(docs) => docs.iter().map(|doc| doc.flat_width()).sum(),
            Doc::Group(doc) => doc.flat_width(),
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
        }
    }
}

impl Debug for Doc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(arg0) => f.debug_tuple("Token").field(&arg0.text()).finish(),
            Self::SoftBreak => write!(f, "SoftBreak"),
            Self::HardBreak => write!(f, "HardBreak"),
            Self::Indent(arg0) => f.debug_tuple("Indent").field(arg0).finish(),
            Self::Concat(arg0) => f.debug_tuple("Concat").field(arg0).finish(),
            Self::Group(arg0) => f.debug_tuple("Group").field(arg0).finish(),
            Self::Trivia(arg0) => f.debug_tuple("Trivia").field(&arg0.to_string()).finish(),
            Self::Space => write!(f, "Space"),
            Self::AlignedSpace(n) => write!(f, "AlignedSpace({n})"),
            Self::Comment(arg0) => f.debug_tuple("LineComment").field(arg0).finish(),
            Self::BlankLines(n) => write!(f, "BlankLines({n})"),
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

/// All nodes that should be printed with an indent.
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

/// All nodes that require a single newline before them.
fn wants_newline_before(node_kind: NodeKind) -> bool {
    use vhdl_syntax::syntax::NodeKind as Nk;
    matches!(
        node_kind,
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
            | Nk::SimpleReleaseAssignment
    )
}

/// Nodes that need a soft break, i.e., a conditional break based on
/// a user-configured maximum line length.
fn wants_softbreak_before(node_kind: NodeKind) -> bool {
    matches!(
        node_kind,
        NodeKind::InterfaceList
            | NodeKind::InterfaceConstantDeclaration
            | NodeKind::InterfaceSignalDeclaration
            | NodeKind::InterfaceVariableDeclaration
            | NodeKind::InterfaceFileDeclaration
            | NodeKind::InterfaceIncompleteTypeDeclaration
            | NodeKind::InterfaceSubprogramDeclaration
            | NodeKind::InterfacePackageDeclaration
            | NodeKind::PortClauseEpilogue
            | NodeKind::GenericClauseEpilogue
    )
}

impl Doc {
    pub fn from_node(node: SyntaxNode, alignment: &AlignmentMap) -> Doc {
        let mut builder = DocBuilder::new();
        let preorder = PreorderWithTokens::new(node);
        let mut pending_break = None;

        for event in preorder {
            match event {
                WalkEvent::Enter(SyntaxElement::Node(node)) => {
                    if wants_newline_before(node.kind()) {
                        let has_preceding_content = node
                            .first_token()
                            .and_then(|tok| tok.prev_token())
                            .is_some();
                        if has_preceding_content {
                            pending_break = Some(Doc::HardBreak);
                        }
                    }
                    builder.start_concat();
                    if wants_softbreak_before(node.kind()) && pending_break.is_none() {
                        pending_break = Some(Doc::SoftBreak)
                    }
                }
                WalkEvent::Enter(SyntaxElement::Token(token)) => {
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
                                            if sep.blank_lines() > 0 {
                                                builder.push(Doc::BlankLines(sep.blank_lines()));
                                            }
                                            builder.push(Doc::HardBreak);
                                        } else {
                                            builder.soft_break();
                                        }
                                    }
                                    builder.comment(DocComment::Block(comment.clone()));
                                }
                                TriviaPiece::LineComment(comment) => {
                                    if let Some(sep) = last_sep.take() {
                                        if sep.hard {
                                            if sep.blank_lines() > 0 {
                                                builder.push(Doc::BlankLines(sep.blank_lines()));
                                            }
                                            builder.push(Doc::HardBreak);
                                        } else {
                                            builder.soft_break();
                                        }
                                    }
                                    builder.comment(DocComment::Line(comment.clone()));
                                }
                                TriviaPiece::Unexpected(_) => unimplemented!("Unexpected trivia"),
                            }
                        }
                        // The trailing separator after the last comment owns the boundary
                        // (last comment -> token). If it is hard, pending_break is dropped
                        // so the formatter does not emit a second HardBreak for the same boundary.
                        // Soft-only separators are dropped — the comment already provides spacing.
                        let trailing_was_hard = if let Some(sep) = last_sep.take()
                            && sep.hard
                        {
                            if sep.blank_lines() > 0 {
                                builder.push(Doc::BlankLines(sep.blank_lines()));
                            }
                            builder.push(Doc::HardBreak);
                            true
                        } else {
                            false
                        };
                        if trailing_was_hard {
                            pending_break = None;
                        }
                    } else {
                        let bl = count_user_blank_lines(token.leading_trivia());
                        if bl > 0 {
                            builder.push(Doc::BlankLines(bl));
                        }
                    }

                    if let Some(pending_break) = pending_break.take() {
                        builder.push(pending_break);
                    }
                    if let Some(alignment) = alignment.get(&token) {
                        builder.push(Doc::AlignedSpace(alignment));
                    }
                    if !token.leading_trivia().contains_comments() {
                        builder.trivia(token.leading_trivia().clone());
                    }
                    builder.token(token.clone());
                }
                WalkEvent::Leave(SyntaxElement::Token(_)) => {}
                WalkEvent::Leave(SyntaxElement::Node(node)) => {
                    builder.end_concat();
                    if matches!(
                        node.kind(),
                        NodeKind::InterfaceList
                            | NodeKind::GenericClause
                            | NodeKind::PortClause
                            | NodeKind::ParenthesizedInterfaceList
                            | NodeKind::Declarations
                            | NodeKind::RecordElementDeclarations
                    ) {
                        builder.embed_in_group();
                    }
                    if indents(&node) {
                        builder.embed_in_indent();
                    }
                }
            }
        }
        builder.build()
    }
}
