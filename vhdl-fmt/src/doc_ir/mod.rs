use std::{collections::HashMap, fmt::Debug};

use vhdl_syntax::syntax::{
    NodeKind,
    node::{SyntaxElement, SyntaxNode, SyntaxToken},
    visitor::{PreorderWithTokens, WalkEvent},
};

use crate::doc_ir::builder::DocBuilder;
mod builder;

#[derive(Clone)]
pub enum Doc {
    /// The basic element of text
    Token(SyntaxToken),
    /// Mandatory newline
    HardBreak,
    /// Indent the
    Indent(Vec<Doc>),
    /// Sequence of docs. Rendered without any special consideration.
    Concat(Vec<Doc>),
    /// Group: try to keep flat if it fits max width
    Group(Vec<Doc>),
    /// Optional break: space if fits, newline + indent otherwise
    SoftBreak,
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
            | InterfaceConstantDeclaration
            | InterfaceSignalDeclaration
            | InterfaceVariableDeclaration
            | InterfaceFileDeclaration
            | InterfaceIncompleteTypeDeclaration
            | InterfaceSubprogramDeclaration
            | InterfacePackageDeclaration
    )
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BoundaryDecision {
    /// Add a space before a token
    Space,
    /// Add a newline with given indent
    Newline { indent: usize },
}

struct ResolveState {
    plan: HashMap<usize, BoundaryDecision>,
    pending: Option<BoundaryDecision>,
    indent: usize,
}

impl ResolveState {
    pub fn new() -> ResolveState {
        ResolveState {
            plan: HashMap::new(),
            pending: None,
            indent: 0,
        }
    }
}

impl Doc {
    pub fn from_node(node: SyntaxNode) -> Doc {
        let mut builder = DocBuilder::new();
        let preorder = PreorderWithTokens::new(node);
        for event in preorder {
            match event {
                WalkEvent::Enter(SyntaxElement::Node(node)) => {
                    if indents(&node) {
                        builder.indent();
                    }
                    if wants_newline_before(node.kind()) {
                        builder.hard_break();
                    }
                }
                WalkEvent::Enter(SyntaxElement::Token(token)) => builder.push(token),
                WalkEvent::Leave(SyntaxElement::Token(_)) => {}
                WalkEvent::Leave(SyntaxElement::Node(node)) => {
                    if indents(&node) {
                        builder.dedent();
                    }
                }
            }
        }
        builder.build()
    }

    pub fn resolve_layout(self) -> HashMap<usize, BoundaryDecision> {
        let mut state = ResolveState::new();
        self.resolve_layout_inner(&mut state);
        state.plan
    }

    fn resolve_layout_inner(self, state: &mut ResolveState) {
        match self {
            Doc::Token(syntax_token) => {
                if let Some(pending) = state.pending.take() {
                    state.plan.insert(syntax_token.text_pos(), pending);
                }
            }
            Doc::HardBreak => {
                state.pending = Some(BoundaryDecision::Newline {
                    indent: state.indent,
                })
            }
            Doc::Indent(docs) => {
                state.indent += 1;
                for doc in docs {
                    doc.resolve_layout_inner(state);
                }
                state.indent -= 1;
            }
            Doc::Concat(docs) => {
                for doc in docs {
                    doc.resolve_layout_inner(state);
                }
            }
            Doc::SoftBreak => todo!(),
            Doc::Group(_) => todo!(),
        }
    }
}
