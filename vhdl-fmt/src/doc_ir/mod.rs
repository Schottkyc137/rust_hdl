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
    use vhdl_syntax::syntax::NodeKind::*;
    match node.kind() {
        ContextClause
            if node
                .parent()
                .is_some_and(|par| par.kind() == ContextDeclaration) =>
        {
            true
        }
        Declarations
        | ConcurrentStatements
        | SequentialStatements
        | BlockConfigurationItems
        | BlockHeader
        | GenerateStatementBody
        | CaseGenerateAlternative
        | CaseStatementAlternative
        | ComponentConfigurationItems
        | ComponentDeclarationItems
        | ComponentInstantiationItems
        | CompoundConfigurationSpecificationItems
        | ConfigurationDeclarationItems
        | EntityHeader
        | PackageHeader
        | UnitDeclarations
        | RecordElementDeclarations
        | InterfaceList => true,
        _ => false,
    }
}

/// All nodes that require a single newline before them.
fn wants_newline_before(node_kind: NodeKind) -> bool {
    use vhdl_syntax::syntax::NodeKind::*;
    matches!(
        node_kind,
        AliasDeclaration
            | DeclarationStatementSeparator
            | SemiColonTerminatedBindingIndication
            | UseClause
            | SubprogramDeclaration
            | SubprogramBody
            | SubprogramInstantiationDeclaration
            | PackageDeclaration
            | PackageBody
            | PackageInstantiationDeclaration
            | FullTypeDeclaration
            | IncompleteTypeDeclaration
            | SubtypeDeclaration
            | ConstantDeclaration
            | SignalDeclaration
            | VariableDeclaration
            | SharedVariableDeclaration
            | FileDeclaration
            | ComponentDeclaration
            | AttributeDeclaration
            | GroupTemplateDeclaration
            | GroupDeclaration
            | AttributeSpecification
            | SimpleConfigurationSpecification
            | CompoundConfigurationSpecification
            | DisconnectionSpecification
            | PslPropertyDeclaration
            | PslSequenceDeclaration
            | PslClockDeclaration
            | GenericClause
            | PortClause
            | GenericMapAspect
            | PortMapAspect
            | BlockHeader
            | GenerateStatementBody
            | CaseGenerateAlternative
            | CaseStatementAlternative
            | SemiColonTerminatedVerificationUnitBindingIndication
            | BlockConfiguration
            | BlockStatement
            | ProcessStatement
            | ConcurrentAssertionStatement
            | ComponentInstantiationStatement
            | ConcurrentSelectedSignalAssignment
            | ConcurrentConditionalSignalAssignment
            | ConcurrentSimpleSignalAssignment
            | ConcurrentProcedureCallOrComponentInstantiationStatement
            | ForGenerateStatement
            | IfGenerateElsif
            | IfGenerateElse
            | CaseGenerateStatement
            | PslDirective
            | WaitStatement
            | AssertionStatement
            | ReportStatement
            | ProcedureCallStatement
            | SimpleVariableAssignment
            | ConditionalVariableAssignment
            | SelectedVariableAssignment
            | IfStatement
            | IfStatementElsif
            | IfStatementElse
            | CaseStatement
            | LoopStatement
            | NextStatement
            | ExitStatement
            | ReturnStatement
            | NullStatement
            | PackageBodyDeclaration
            | BlockPreamble
            | PackagePreamble
            | IfStatementPreamble
            | PackageBodyPreamble
            | ArchitecturePreamble
            | CaseStatementPreamble
            | LoopStatementPreamble
            | SubprogramBodyPreamble
            | ProcessStatementPreamble
            | EntityDeclarationPreamble
            | ProtectedTypeBodyPreamble
            | BlockConfigurationPreamble
            | ContextDeclarationPreamble
            | IfGenerateStatementPreamble
            | ComponentDeclarationPreamble
            | ForGenerateStatementPreamble
            | RecordTypeDefinitionPreamble
            | CaseGenerateStatementPreamble
            | ComponentConfigurationPreamble
            | ConfigurationDeclarationPreamble
            | ProtectedTypeDeclarationPreamble
            | BlockEpilogue
            | PackageEpilogue
            | IfStatementEpilogue
            | PackageBodyEpilogue
            | ArchitectureEpilogue
            | CaseStatementEpilogue
            | LoopStatementEpilogue
            | SubprogramBodyEpilogue
            | ProcessStatementEpilogue
            | EntityDeclarationEpilogue
            | ProtectedTypeBodyEpilogue
            | BlockConfigurationEpilogue
            | ContextDeclarationEpilogue
            | IfGenerateStatementEpilogue
            | ComponentDeclarationEpilogue
            | ForGenerateStatementEpilogue
            | RecordTypeDefinitionEpilogue
            | CaseGenerateStatementEpilogue
            | GenerateStatementBodyEpilogue
            | ComponentConfigurationEpilogue
            | PhysicalTypeDefinitionEpilogue
            | ConfigurationDeclarationEpilogue
            | ProtectedTypeDeclarationEpilogue
            | PrimaryUnitDeclaration
            | SecondaryUnitDeclaration
            | ElementDeclaration
            | SimpleWaveformAssignment
            | SimpleForceAssignment
            | SimpleReleaseAssignment
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
                    if token.all_leading_trivia().contains_comments() {
                        let mut last_sep = None;
                        for triv in token.all_leading_trivia() {
                            match triv {
                                TriviaPiece::HorizontalTabs(_)
                                | TriviaPiece::Spaces(_)
                                | TriviaPiece::NonBreakingSpaces(_) => match last_sep {
                                    None => last_sep = Some(Doc::SoftBreak),
                                    _ => {}
                                },
                                TriviaPiece::BlockComment(comment) => {
                                    if let Some(sep) = last_sep.take() {
                                        builder.push(sep);
                                    }
                                    builder.comment(DocComment::Block(comment));
                                }
                                TriviaPiece::LineComment(comment) => {
                                    if let Some(sep) = last_sep.take() {
                                        builder.push(sep);
                                    }
                                    builder.comment(DocComment::Line(comment));
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
                    if !token.all_leading_trivia().contains_comments() {
                        builder.trivia(token.all_leading_trivia());
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
