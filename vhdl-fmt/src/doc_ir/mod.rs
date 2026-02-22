pub(crate) mod boundary;
mod builder;
pub(crate) mod resolve;

use crate::doc_ir::builder::DocBuilder;
use std::{fmt::Debug, usize};
use vhdl_syntax::{
    syntax::{
        NodeKind,
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

#[derive(Clone)]
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
            Doc::Trivia(trivia) => {
                if trivia.has_newline() {
                    None
                } else {
                    Some(trivia.byte_len())
                }
            }
            Doc::Comment(comment) => Some(comment.byte_len()),
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
            Self::Comment(arg0) => f.debug_tuple("LineComment").field(arg0).finish(),
        }
    }
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
    pub fn from_node(node: SyntaxNode) -> Doc {
        let mut builder = DocBuilder::new();
        let preorder = PreorderWithTokens::new(node);
        let mut pending_break = None;

        for event in preorder {
            match event {
                WalkEvent::Enter(SyntaxElement::Node(node)) => {
                    if wants_newline_before(node.kind()) {
                        pending_break = Some(Doc::HardBreak);
                    }
                    builder.start_concat();
                    if wants_softbreak_before(node.kind()) {
                        if pending_break.is_none() {
                            pending_break = Some(Doc::SoftBreak)
                        }
                    }
                }
                WalkEvent::Enter(SyntaxElement::Token(token)) => {
                    let leading = token.leading_trivia();
                    let comment_index = leading
                        .iter()
                        .position(|triv| triv.is_comment())
                        .unwrap_or(leading.len());
                    let (non_comments, maybe_comments) = leading.split_at(comment_index);
                    let newline_count: usize =
                        non_comments.iter().map(|piece| piece.newline_count()).sum();
                    match pending_break {
                        Some(Doc::Space) => {
                            // remove all leading line break trivia
                            // ignore the non_comment trivia
                        }
                        Some(Doc::HardBreak) => {
                            // remove one newline from the user trivia (if there is one)
                            // and treat the remainder as user-supplied extra
                            for _ in 0..newline_count.saturating_sub(1) {
                                builder.push(Doc::HardBreak);
                            }
                        }
                        Some(Doc::SoftBreak) => {
                            // TODO
                            // remove one newline from the user trivia and treat the remainder as optional hard breaks if soft break is taken else none.
                        }
                        Some(_) => panic!(
                            "Pending break should never be anything but space, hard break or soft break"
                        ),
                        None => {
                            // unknown -> keep the trivia as-is
                        }
                    }
                    // then: push all comments.
                    //   On line comments, add a trailing newline.
                    let last_comment_index =
                        maybe_comments.iter().rposition(|piece| piece.is_comment());
                    if let Some(last_comment_index) = last_comment_index {
                        let mut last_sep = None;
                        for triv in &maybe_comments[..=last_comment_index] {
                            match triv {
                                TriviaPiece::HorizontalTabs(_)
                                | TriviaPiece::Spaces(_)
                                | TriviaPiece::NonBreakingSpaces(_) => match last_sep {
                                    None => last_sep = Some(Doc::SoftBreak),
                                    _ => {}
                                },
                                TriviaPiece::BlockComment(comment) => {
                                    builder.comment(DocComment::Block(comment.clone()));
                                    // Always hard bread after line comment
                                    builder.push(Doc::HardBreak);
                                }
                                TriviaPiece::LineComment(comment) => {
                                    if let Some(sep) = last_sep.take() {
                                        builder.push(sep);
                                    }
                                    builder.comment(DocComment::Line(comment.clone()));
                                }
                                TriviaPiece::VerticalTabs(_)
                                | TriviaPiece::CarriageReturnLineFeeds(_)
                                | TriviaPiece::LineFeeds(_)
                                | TriviaPiece::CarriageReturns(_)
                                | TriviaPiece::FormFeeds(_) => last_sep = Some(Doc::HardBreak),
                                TriviaPiece::Unexpected(_) => unimplemented!("Unexpected trivia"),
                            }
                        }
                        if let Some(sep) = last_sep.take() {
                            if matches!(sep, Doc::HardBreak) {
                                builder.push(sep);
                            }
                        }
                    }
                    if token.kind() == TokenKind::RightPar
                        && token.parent().kind() == NodeKind::ParenthesizedInterfaceList
                    {
                        builder.soft_break();
                    } else if let Some(pendind_break) = pending_break.take() {
                        builder.push(pendind_break);
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
