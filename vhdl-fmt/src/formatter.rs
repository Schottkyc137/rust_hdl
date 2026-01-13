use vhdl_syntax::{
    syntax::{
        NodeKind,
        node::{SyntaxNode, SyntaxToken},
        rewrite::{TokenRewrite, TokenRewriteAction, TokenRewriter},
    },
    tokens::{Trivia, TriviaPiece},
};

use crate::{
    config::{Config, Indentation, NewlineStyle},
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

/// Trims trailing whitespace from the provided trivia.
fn trim_trailing_ws(line: &[TriviaPiece]) -> &[TriviaPiece] {
    if let Some(pos) = line.iter().rposition(|t| !t.is_space_or_tab()) {
        &line[..=pos]
    } else {
        line
    }
}

/// Trims leading whitespace from the provided trivia.
fn trim_leading_ws(line: &[TriviaPiece]) -> &[TriviaPiece] {
    if let Some(pos) = line.iter().position(|t| !t.is_space_or_tab()) {
        &line[pos..]
    } else {
        line
    }
}

///  
fn emit_line(
    out: &mut Trivia,
    line: &[TriviaPiece],
    newline: &TriviaPiece,
    indent: &Indentation,
    indent_level: usize,
) {
    if indent_level > 0 {
        out.push(indent.to_trivia(indent_level));
    }
    out.extend(line.iter().cloned());
    out.push(newline.clone());
}

fn normalize_line(line: &[TriviaPiece], is_first: bool) -> &[TriviaPiece] {
    let line = trim_trailing_ws(line);

    if is_first {
        line
    } else {
        trim_leading_ws(line)
    }
}

/// Normalizes trivia:
///
/// - No whitespace after newlines
fn normalize_trivia(trivia: &Trivia, indent: &Indentation, indent_level: usize) -> Trivia {
    let mut out = Trivia::default();
    let mut first_newline = true;

    for chunk in trivia.split_inclusive(TriviaPiece::is_newline) {
        if let Some(newline) = chunk.last().filter(|piece| piece.is_newline()) {
            let content = &chunk[..chunk.len() - 1];
            let normalized = normalize_line(content, first_newline);
            first_newline = false;

            emit_line(&mut out, normalized, newline, indent, indent_level);
        } else {
            if !(!first_newline && chunk.iter().all(TriviaPiece::is_space_or_tab)) {
                out.extend(chunk.iter().cloned());
            }
        }
    }

    out
}

fn ensure_newlines(trivia: &mut Trivia, n: usize, _newline_style: NewlineStyle) {
    // TODO: respect newline style
    let count_of_newlines: usize = trivia
        .iter()
        .filter_map(|piece| match piece {
            TriviaPiece::CarriageReturnLineFeeds(n) => Some(n),
            TriviaPiece::LineFeeds(n) => Some(n),
            TriviaPiece::CarriageReturns(n) => Some(n),
            TriviaPiece::FormFeeds(n) => Some(n),
            TriviaPiece::VerticalTabs(n) => Some(n),
            _ => None,
        })
        .copied()
        .sum();
    if count_of_newlines < n {
        trivia.push(TriviaPiece::LineFeeds(n - count_of_newlines));
    }
}

impl Formatter {
    pub fn new(config: Config) -> Formatter {
        Formatter {
            config,
            state: State::new(),
        }
    }

    fn format_token(&mut self, token: &SyntaxToken) -> SyntaxToken {
        let mut tok = token.clone();

        let mut leading_trivia = self
            .state
            .take_previoud_trailing_trivia()
            .unwrap_or_default();

        leading_trivia.append(&mut tok.leading_trivia());

        leading_trivia = normalize_trivia(
            &leading_trivia,
            &self.config.indentationn,
            self.state.current_indent(),
        );

        if let Some(separator) = self.state.get_and_reset_pending_separator()
            // prevent newlines at the beginning
            && tok.prev_token().is_some()
        {
            match separator {
                RegionSeparator::Space => {
                    leading_trivia.push(TriviaPiece::Spaces(1));
                }
                RegionSeparator::Newline => {
                    ensure_newlines(&mut leading_trivia, 1, self.config.newline_style);
                    if self.state.current_indent() != 0 {
                        leading_trivia.push(
                            self.config
                                .indentationn
                                .to_trivia(self.state.current_indent()),
                        );
                    }
                }
            }
        }

        // Store the previous trivia and reset trailing trivia of the token.
        // This means that for formatting, we only have to deal with trailing trivia.
        // TODO: Improve this by introducing an EOF token.
        if !tok.next_token().is_none() {
            self.state
                .set_previous_trailing_trivia(tok.trailing_trivia());
            tok = tok.clone_with_trivia(leading_trivia, Trivia::default())
        } else {
            tok = tok.clone_with_leading_trivia(leading_trivia)
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

/// All nodes that should be printed with an indent.
fn indents(node: &SyntaxNode) -> bool {
    use NodeKind::*;
    match node.kind() {
        ContextClause
            if node
                .parent()
                .is_some_and(|par| par.kind() == NodeKind::ContextDeclaration) =>
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
    use NodeKind::*;
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

impl<'a> TokenRewrite for FormattingTokenRewriter<'a> {
    // Add a leading trivia piece before the next token
    fn enter(&mut self, node: &SyntaxNode) {
        if indents(&node) {
            self.indent();
        }
        if wants_newline_before(node.kind()) {
            self.set_pending_newline();
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
        if indents(&node) {
            self.dedent();
        }
    }
}
