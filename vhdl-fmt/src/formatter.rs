use vhdl_syntax::{
    syntax::{
        NodeKind,
        node::{SyntaxNode, SyntaxToken},
        rewrite::{TokenRewrite, TokenRewriteAction, TokenRewriter},
    },
    tokens::{Trivia, TriviaPiece},
};

use crate::{
    config::{Config, NewlineStyle},
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

/// Normalizes trivia:
///
/// - No whitespace after newlines
fn normalize_trivia(trivia: &Trivia) -> Trivia {
    let mut new_trivia = Trivia::default();
    for line in trivia.split_inclusive(TriviaPiece::is_newline) {
        if line.last().is_some_and(TriviaPiece::is_newline) {
            if let Some(pos) = line[..line.len()-1]
                .iter()
                .rposition(|item| !item.is_space_or_tab())
            {
                new_trivia.extend(line[..pos+1].to_owned());
            }

            new_trivia.push(line.last().unwrap().clone());
        } else {
            new_trivia.extend(line.to_owned());
        }
    }
    new_trivia
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
        // let mut tok = ALL_RULES.iter().fold(token.clone(), |token, rule| {
        //     if (rule.applies)(&token, &mut self.state) {
        //         (rule.apply)(&token, &mut self.state, &self.config)
        //     } else {
        //         token.clone()
        //     }
        // });
        let mut tok = token.clone();

        let mut leading_trivia = self
            .state
            .take_previoud_trailing_trivia()
            .unwrap_or_default();

        leading_trivia.append(&mut tok.leading_trivia());

        leading_trivia = normalize_trivia(&leading_trivia);

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
fn indents(node_kind: NodeKind) -> bool {
    use NodeKind::*;
    matches!(
        node_kind,
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
            | ContextClause
            | EntityHeader
            | PackageHeader
            | UnitDeclarations
            | RecordElementDeclarations
            | InterfaceList
    )
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
        if indents(node.kind()) {
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
        if indents(node.kind()) {
            self.dedent();
        }
    }
}
