use std::{collections::HashMap, fmt::Debug, mem::take, usize};

use vhdl_syntax::{
    syntax::{
        NodeKind,
        node::{SyntaxElement, SyntaxNode, SyntaxToken},
        visitor::{PreorderWithTokens, WalkEvent},
    },
    tokens::{TokenKind, Trivia, TriviaPiece},
};

use crate::{config::Config, doc_ir::builder::DocBuilder};
mod builder;

#[derive(Clone)]
pub enum Doc {
    /// The basic element of text
    Token(SyntaxToken),
    /// Mandatory newline
    HardBreak,
    /// Indent the following docs
    Indent(Box<Doc>),
    /// Sequence of docs. Rendered without any special consideration.
    Concat(Vec<Doc>),
    /// Group: try to keep flat if it fits max width
    Group(Box<Doc>),
    /// Optional break: space if fits, newline + indent otherwise
    SoftBreak,
    /// A forced space
    Space,
    /// A comment
    Comment(Trivia),
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
            Doc::Comment(trivia) => {
                if trivia.count_newlines() > 0 {
                    None
                } else {
                    Some(trivia.byte_len())
                }
            }
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
            Self::Comment(arg0) => f.debug_tuple("Comment").field(&arg0.to_string()).finish(),
            Self::Space => write!(f, "Space"),
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BoundaryDecision {
    /// No trivia before a token
    Empty,
    /// Add a space before a token
    Space,
    /// Add a newline with blank lines and given indent
    Newline { blank_lines: usize, indent: usize },
}

pub struct TokenFormatting {
    pub boundary_decision: BoundaryDecision,
    pub leading_comments: Trivia,
    pub trailing_comments: Trivia,
}

struct ResolveState {
    plan: HashMap<usize, TokenFormatting>,
    pending: Option<BoundaryDecision>,
    pending_comments: Trivia,
    indent: usize,
    /// The current column
    column: usize,
}

impl ResolveState {
    pub fn new() -> ResolveState {
        ResolveState {
            plan: HashMap::new(),
            pending: None,
            indent: 0,
            column: 0,
            pending_comments: Trivia::default(),
        }
    }
}

impl Doc {
    pub fn from_node(node: SyntaxNode) -> Doc {
        let mut builder = DocBuilder::new();
        let preorder = PreorderWithTokens::new(node);

        let mut trivia_decision = false;

        for event in preorder {
            match event {
                WalkEvent::Enter(SyntaxElement::Node(node)) => {
                    if wants_newline_before(node.kind()) {
                        builder.hard_break();
                        let additional_newlines = node
                            .first_token()
                            .map(|tok| tok.all_leading_trivia().count_newlines())
                            .unwrap_or_default();
                        for _ in 0..additional_newlines {
                            builder.hard_break();
                        }
                        trivia_decision = true;
                    }
                    builder.start_concat();
                    if wants_softbreak_before(node.kind()) {
                        builder.soft_break();
                        trivia_decision = true;
                    }
                }
                WalkEvent::Enter(SyntaxElement::Token(token)) => {
                    if token.kind() == TokenKind::RightPar
                        && token.parent().kind() == NodeKind::ParenthesizedInterfaceList
                    {
                        builder.soft_break();
                    }
                    let leading_trivia = token.all_leading_trivia();
                    let begin_index = leading_trivia.iter().position(TriviaPiece::is_comment);
                    let end_index = leading_trivia.iter().rposition(TriviaPiece::is_comment);
                    if let (Some(begin), Some(end)) = (begin_index, end_index) {
                        builder.comment(leading_trivia[begin..=end].into());
                        if matches!(
                            leading_trivia.iter().as_slice()[end],
                            TriviaPiece::LineComment(_)
                        ) {
                            builder.hard_break();
                        }
                    }
                    if trivia_decision {
                        trivia_decision = false;
                    } else {
                        if token.all_leading_trivia().has_spaces_or_tabs() {
                            builder.space();
                        }
                    }
                    builder.push(token.clone());
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

    pub fn resolve_layout(self, config: &Config) -> HashMap<usize, TokenFormatting> {
        let mut state = ResolveState::new();
        self.resolve_layout_inner(config, &mut state, true);
        state.plan
    }

    fn resolve_layout_inner(self, config: &Config, state: &mut ResolveState, flat: bool) {
        match self {
            Doc::Token(syntax_token) => {
                let pending = state.pending.take().unwrap_or(BoundaryDecision::Empty);
                state.plan.insert(
                    syntax_token.text_pos(),
                    TokenFormatting {
                        boundary_decision: pending,
                        leading_comments: take(&mut state.pending_comments),
                        trailing_comments: Trivia::new(),
                    },
                );
                match pending {
                    BoundaryDecision::Space => state.column += 1,
                    BoundaryDecision::Newline {
                        blank_lines: _,
                        indent,
                    } => state.column = indent,
                    BoundaryDecision::Empty => {}
                }
                state.column += syntax_token.text().len();
            }
            Doc::HardBreak => {
                if let Some(pending) = &mut state.pending {
                    match pending {
                        BoundaryDecision::Newline {
                            blank_lines,
                            indent: _,
                        } => *blank_lines += 1,
                        BoundaryDecision::Empty | BoundaryDecision::Space => {
                            state.pending = Some(BoundaryDecision::Newline {
                                blank_lines: 0,
                                indent: state.indent,
                            })
                        }
                    }
                }
            }
            Doc::Indent(doc) => {
                state.indent += config.indentation.width;
                doc.resolve_layout_inner(config, state, flat);
                state.indent -= config.indentation.width;
            }
            Doc::SoftBreak => {
                state.pending = if flat {
                    Some(BoundaryDecision::Space)
                } else {
                    Some(BoundaryDecision::Newline {
                        indent: state.indent,
                        blank_lines: 0,
                    })
                };
            }
            Doc::Group(doc) => {
                let layout_as_flat = if let Some(flat_width) = doc.flat_width() {
                    flat && state.column + flat_width <= config.max_line_length
                } else {
                    false
                };
                doc.resolve_layout_inner(config, state, layout_as_flat);
            }
            Doc::Concat(docs) => {
                for doc in docs {
                    doc.resolve_layout_inner(config, state, flat);
                }
            }
            Doc::Comment(mut trivia) => state.pending_comments.append(&mut trivia),
            Doc::Space => state.pending = Some(BoundaryDecision::Space),
        }
    }
}
