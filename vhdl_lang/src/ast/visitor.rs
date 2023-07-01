use crate::ast::visitor::VisitorResult::{Continue, Skip, Stop};
use crate::ast::*;
use std::ops::Deref;

#[derive(PartialEq)]
pub enum VisitorResult {
    Continue,
    Stop,
    Skip,
}

pub trait Visitor {
    fn visit_attribute_name(&self, _node: &AttributeName) -> VisitorResult {
        Continue
    }
    fn visit_attribute_designator(&self, _node: &AttributeDesignator) -> VisitorResult {
        Continue
    }
    fn visit_external_path(&self, _node: &ExternalPath) -> VisitorResult {
        Continue
    }
    fn visit_external_name(&self, _node: &ExternalName) -> VisitorResult {
        Continue
    }
    fn visit_name(&self, _node: &Name) -> VisitorResult {
        Continue
    }
    fn visit_selected_name(&self, _node: &SelectedName) -> VisitorResult {
        Continue
    }
    fn visit_call_or_indexed(&self, _node: &CallOrIndexed) -> VisitorResult {
        Continue
    }
    fn visit_choice(&self, _node: &Choice) -> VisitorResult {
        Continue
    }
    fn visit_element_association(&self, _node: &ElementAssociation) -> VisitorResult {
        Continue
    }
    fn visit_actual_part(&self, _node: &ActualPart) -> VisitorResult {
        Continue
    }
    fn visit_association_element(&self, _node: &AssociationElement) -> VisitorResult {
        Continue
    }
    fn visit_abstract_literal(&self, _node: &AbstractLiteral) -> VisitorResult {
        Continue
    }
    fn visit_bit_string(&self, _node: &BitString) -> VisitorResult {
        Continue
    }
    fn visit_physical_literal(&self, _node: &PhysicalLiteral) -> VisitorResult {
        Continue
    }
    fn visit_literal(&self, _node: &Literal) -> VisitorResult {
        Continue
    }
    fn visit_allocator(&self, _node: &Allocator) -> VisitorResult {
        Continue
    }
    fn visit_qualified_expression(&self, _node: &QualifiedExpression) -> VisitorResult {
        Continue
    }
    fn visit_expression(&self, _node: &Expression) -> VisitorResult {
        Continue
    }
    fn visit_ident(&self, _node: &Ident) -> VisitorResult {
        Continue
    }
    fn visit_discrete_range(&self, _node: &DiscreteRange) -> VisitorResult {
        Continue
    }
    fn visit_range_constraint(&self, _node: &RangeConstraint) -> VisitorResult {
        Continue
    }
    fn visit_range(&self, _node: &Range) -> VisitorResult {
        Continue
    }
    fn visit_element_constraint(&self, _node: &ElementConstraint) -> VisitorResult {
        Continue
    }
    fn visit_subtype_constraint(&self, _node: &SubtypeConstraint) -> VisitorResult {
        Continue
    }
    fn visit_record_element_resolution(&self, _node: &RecordElementResolution) -> VisitorResult {
        Continue
    }
    fn visit_resolution_indication(&self, _node: &ResolutionIndication) -> VisitorResult {
        Continue
    }
    fn visit_type_mark(&self, _node: &TypeMark) -> VisitorResult {
        Continue
    }
    fn visit_subtype_indication(&self, _node: &SubtypeIndication) -> VisitorResult {
        Continue
    }
    fn visit_array_index(&self, _node: &ArrayIndex) -> VisitorResult {
        Continue
    }
    fn visit_element_declaration(&self, _node: &ElementDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_protected_type_declarative_item(
        &self,
        _node: &ProtectedTypeDeclarativeItem,
    ) -> VisitorResult {
        Continue
    }
    fn visit_designator(&self, _node: &Designator) -> VisitorResult {
        Continue
    }
    fn visit_alias_declaration(&self, _node: &AliasDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_attribute_declaration(&self, _node: &AttributeDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_entity_tag(&self, _node: &EntityTag) -> VisitorResult {
        Continue
    }
    fn visit_entity_name(&self, _node: &EntityName) -> VisitorResult {
        Continue
    }
    fn visit_attribute_specification(&self, _node: &AttributeSpecification) -> VisitorResult {
        Continue
    }
    fn visit_attribute(&self, _node: &Attribute) -> VisitorResult {
        Continue
    }
    fn visit_protected_type_declaration(&self, _node: &ProtectedTypeDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_protected_type_body(&self, _node: &ProtectedTypeBody) -> VisitorResult {
        Continue
    }
    fn visit_physical_type_declaration(&self, _node: &PhysicalTypeDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_enumeration_literal(&self, _node: &EnumerationLiteral) -> VisitorResult {
        Continue
    }
    fn visit_type_definition(&self, _node: &TypeDefinition) -> VisitorResult {
        Continue
    }
    fn visit_type_declaration(&self, _node: &TypeDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_object_declaration(&self, _node: &ObjectDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_file_declaration(&self, _node: &FileDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_subprogram_designator(&self, _node: &SubprogramDesignator) -> VisitorResult {
        Continue
    }
    fn visit_procedure_specification(&self, _node: &ProcedureSpecification) -> VisitorResult {
        Continue
    }
    fn visit_function_specification(&self, _node: &FunctionSpecification) -> VisitorResult {
        Continue
    }
    fn visit_subprogram_body(&self, _node: &SubprogramBody) -> VisitorResult {
        Continue
    }
    fn visit_signature(&self, _node: &Signature) -> VisitorResult {
        Continue
    }
    fn visit_subprogram_declaration(&self, _node: &SubprogramDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_interface_file_declaration(&self, _node: &InterfaceFileDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_interface_object_declaration(
        &self,
        _node: &InterfaceObjectDeclaration,
    ) -> VisitorResult {
        Continue
    }
    fn visit_subprogram_default(&self, _node: &SubprogramDefault) -> VisitorResult {
        Continue
    }
    fn visit_interface_package_generic_map_aspect(
        &self,
        _node: &InterfacePackageGenericMapAspect,
    ) -> VisitorResult {
        Continue
    }
    fn visit_interface_package_declaration(
        &self,
        _node: &InterfacePackageDeclaration,
    ) -> VisitorResult {
        Continue
    }
    fn visit_interface_declaration(&self, _node: &InterfaceDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_port_clause(&self, _node: &PortClause) -> VisitorResult {
        Continue
    }
    fn visit_component_declaration(&self, _node: &ComponentDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_declaration(&self, _node: &Declaration) -> VisitorResult {
        Continue
    }
    fn visit_wait_statement(&self, _node: &WaitStatement) -> VisitorResult {
        Continue
    }
    fn visit_assert_statement(&self, _node: &AssertStatement) -> VisitorResult {
        Continue
    }
    fn visit_report_statement(&self, _node: &ReportStatement) -> VisitorResult {
        Continue
    }
    fn visit_target(&self, _node: &Target) -> VisitorResult {
        Continue
    }
    fn visit_waveform_element(&self, _node: &WaveformElement) -> VisitorResult {
        Continue
    }
    fn visit_waveform(&self, _node: &Waveform) -> VisitorResult {
        Continue
    }
    fn visit_delay_mechanism(&self, _node: &DelayMechanism) -> VisitorResult {
        Continue
    }
    fn visit_signal_assignment(&self, _node: &SignalAssignment) -> VisitorResult {
        Continue
    }
    fn visit_signal_force_assignment(&self, _node: &SignalForceAssignment) -> VisitorResult {
        Continue
    }
    fn visit_signal_release_assignment(&self, _node: &SignalReleaseAssignment) -> VisitorResult {
        Continue
    }
    fn visit_variable_assignment(&self, _node: &VariableAssignment) -> VisitorResult {
        Continue
    }
    fn visit_if_statement(&self, _node: &IfStatement) -> VisitorResult {
        Continue
    }
    fn visit_case_statement(&self, _node: &CaseStatement) -> VisitorResult {
        Continue
    }
    fn visit_iteration_scheme(&self, _node: &IterationScheme) -> VisitorResult {
        Continue
    }
    fn visit_loop_statement(&self, _node: &LoopStatement) -> VisitorResult {
        Continue
    }
    fn visit_next_statement(&self, _node: &NextStatement) -> VisitorResult {
        Continue
    }
    fn visit_exit_statement(&self, _node: &ExitStatement) -> VisitorResult {
        Continue
    }
    fn visit_return_statement(&self, _node: &ReturnStatement) -> VisitorResult {
        Continue
    }
    fn visit_sequential_statement(&self, _node: &SequentialStatement) -> VisitorResult {
        Continue
    }
    fn visit_labeled_sequential_statement(
        &self,
        _node: &LabeledSequentialStatement,
    ) -> VisitorResult {
        Continue
    }
    fn visit_block_statement(&self, _node: &BlockStatement) -> VisitorResult {
        Continue
    }
    fn visit_block_header(&self, _node: &BlockHeader) -> VisitorResult {
        Continue
    }
    fn visit_sensitivity_list(&self, _node: &SensitivityList) -> VisitorResult {
        Continue
    }
    fn visit_process_statement(&self, _node: &ProcessStatement) -> VisitorResult {
        Continue
    }
    fn visit_concurrent_procedure_call(&self, _node: &ConcurrentProcedureCall) -> VisitorResult {
        Continue
    }
    fn visit_concurrent_assert_statement(
        &self,
        _node: &ConcurrentAssertStatement,
    ) -> VisitorResult {
        Continue
    }
    fn visit_concurrent_signal_assignment(
        &self,
        _node: &ConcurrentSignalAssignment,
    ) -> VisitorResult {
        Continue
    }
    fn visit_instantiated_unit(&self, _node: &InstantiatedUnit) -> VisitorResult {
        Continue
    }
    fn visit_instantiation_statement(&self, _node: &InstantiationStatement) -> VisitorResult {
        Continue
    }
    fn visit_generate_body(&self, _node: &GenerateBody) -> VisitorResult {
        Continue
    }
    fn visit_for_generate_statement(&self, _node: &ForGenerateStatement) -> VisitorResult {
        Continue
    }
    fn visit_if_generate_statement(&self, _node: &IfGenerateStatement) -> VisitorResult {
        Continue
    }
    fn visit_case_generate_statement(&self, _node: &CaseGenerateStatement) -> VisitorResult {
        Continue
    }
    fn visit_concurrent_statement(&self, _node: &ConcurrentStatement) -> VisitorResult {
        Continue
    }
    fn visit_labeled_concurrent_statement(
        &self,
        _node: &LabeledConcurrentStatement,
    ) -> VisitorResult {
        Continue
    }
    fn visit_library_clause(&self, _node: &LibraryClause) -> VisitorResult {
        Continue
    }
    fn visit_use_clause(&self, _node: &UseClause) -> VisitorResult {
        Continue
    }
    fn visit_context_reference(&self, _node: &ContextReference) -> VisitorResult {
        Continue
    }
    fn visit_context_item(&self, _node: &ContextItem) -> VisitorResult {
        Continue
    }
    fn visit_context_declaration(&self, _node: &ContextDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_package_instantiation(&self, _node: &PackageInstantiation) -> VisitorResult {
        Continue
    }
    fn visit_instantiation_list(&self, _node: &InstantiationList) -> VisitorResult {
        Continue
    }
    fn visit_entity_aspect(&self, _node: &EntityAspect) -> VisitorResult {
        Continue
    }
    fn visit_binding_indication(&self, _node: &BindingIndication) -> VisitorResult {
        Continue
    }
    fn visit_component_specification(&self, _node: &ComponentSpecification) -> VisitorResult {
        Continue
    }
    fn visit_v_unit_binding_indication(&self, _node: &VUnitBindingIndication) -> VisitorResult {
        Continue
    }
    fn visit_configuration_specification(
        &self,
        _node: &ConfigurationSpecification,
    ) -> VisitorResult {
        Continue
    }
    fn visit_configuration_declarative_item(
        &self,
        _node: &ConfigurationDeclarativeItem,
    ) -> VisitorResult {
        Continue
    }
    fn visit_component_configuration(&self, _node: &ComponentConfiguration) -> VisitorResult {
        Continue
    }
    fn visit_configuration_item(&self, _node: &ConfigurationItem) -> VisitorResult {
        Continue
    }
    fn visit_block_configuration(&self, _node: &BlockConfiguration) -> VisitorResult {
        Continue
    }
    fn visit_configuration_declaration(&self, _node: &ConfigurationDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_entity_declaration(&self, _node: &EntityDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_architecture_body(&self, _node: &ArchitectureBody) -> VisitorResult {
        Continue
    }
    fn visit_package_declaration(&self, _node: &PackageDeclaration) -> VisitorResult {
        Continue
    }
    fn visit_package_body(&self, _node: &PackageBody) -> VisitorResult {
        Continue
    }
    fn visit_any_primary_unit(&self, _node: &AnyPrimaryUnit) -> VisitorResult {
        Continue
    }
    fn visit_any_secondary_unit(&self, _node: &AnySecondaryUnit) -> VisitorResult {
        Continue
    }
    fn visit_context_clause(&self, _node: &ContextClause) -> VisitorResult {
        Continue
    }
    fn visit_any_design_unit(&self, _node: &AnyDesignUnit) -> VisitorResult {
        Continue
    }
    fn visit_design_file(&self, _node: &DesignFile) -> VisitorResult {
        Continue
    }
    fn visit_reference(&self, _node: &Reference) -> VisitorResult {Continue}
    fn visit_item_with_pos(&self, _pos: &SrcPos, _node: &dyn ASTNode) -> VisitorResult {
        Continue
    }
    fn visit_item_with_decl(&self, _decl: &Option<EntityId>, _node: &dyn ASTNode) -> VisitorResult {
        Continue
    }
    fn visit_item_with_reference(&self, _ref: &Reference, _node: &dyn ASTNode) -> VisitorResult {
        Continue
    }
}

/// An AST Node has two methods it needs to declare:
/// - A `visit(Visitor)` method.
pub trait ASTNode {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult;

    fn children(&self) -> Vec<&dyn ASTNode>;
}

pub fn walk(node: &dyn ASTNode, visitor: &dyn Visitor) {
    let mut stack: Vec<&dyn ASTNode> = vec![node];
    while !stack.is_empty() {
        let node = stack.pop().unwrap();
        match node.visit(visitor) {
            Stop => return,
            Skip => continue,
            _ => {}
        }
        for child in node.children().into_iter().rev() {
            stack.push(child);
        }
    }
}

#[test]
pub fn test_walker() {
    let tree = Literal::AbstractLiteral(AbstractLiteral::Integer(43));
    struct LiteralVisitor {}
    impl Visitor for LiteralVisitor {
        fn visit_abstract_literal(&self, node: &AbstractLiteral) -> VisitorResult {
            println!("Visited abstract literal {:?}", node);
            Continue
        }

        fn visit_literal(&self, node: &Literal) -> VisitorResult {
            println!("Visited literal {:?}", node);
            Continue
        }
    }
    walk(&tree, &LiteralVisitor {});
}

macro_rules! as_node {
    ($vec:expr) => {
        $vec.iter().map(|f| f as &dyn ASTNode).collect()
    };
}

impl<T: ASTNode> ASTNode for Box<T> {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![self.deref()]
    }
}

impl<T: ASTNode> ASTNode for Option<T> {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match &self {
            None => vec![],
            Some(el) => vec![el],
        }
    }
}

impl<T: ASTNode, U: ASTNode> ASTNode for (T, U) {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.0, &self.1]
    }
}

impl<T: ASTNode> ASTNode for Vec<T> {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        self.iter().map(|f| f as &dyn ASTNode).collect()
    }
}

impl<T: ASTNode> ASTNode for WithPos<T> {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_item_with_pos(&self.pos, &self.item)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.item]
    }
}

impl<T: ASTNode> ASTNode for WithDecl<T> {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_item_with_decl(&self.decl, &self.tree)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.tree]
    }
}

impl<T: ASTNode> ASTNode for WithRef<T> {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_item_with_reference(&self.reference, &self.item)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.reference, &self.item]
    }
}

impl<T: ASTNode> ASTNode for Conditional<T> {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.condition, &self.item]
    }
}

impl<T: ASTNode> ASTNode for Conditionals<T> {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.conditionals, &self.else_item]
    }
}

impl<T: ASTNode> ASTNode for Selection<T> {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.alternatives, &self.expression]
    }
}

impl<T: ASTNode> ASTNode for AssignmentRightHand<T> {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match &self {
            AssignmentRightHand::Simple(expr) => vec![expr],
            AssignmentRightHand::Conditional(conds) => vec![conds],
            AssignmentRightHand::Selected(sel) => vec![sel],
        }
    }
}

impl<T: ASTNode> ASTNode for Alternative<T> {
    fn visit(&self, _visitor: &dyn Visitor) -> VisitorResult {
        Continue
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.choices, &self.item]
    }
}

impl ASTNode for DesignFile {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_design_file(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        as_node!(self.design_units)
    }
}

impl ASTNode for AnyDesignUnit {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_any_design_unit(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            AnyDesignUnit::Primary(unit) => vec![unit],
            AnyDesignUnit::Secondary(unit) => vec![unit],
        }
    }
}

impl ASTNode for AnyPrimaryUnit {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_any_primary_unit(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            AnyPrimaryUnit::Entity(decl) => vec![decl],
            AnyPrimaryUnit::Configuration(decl) => vec![decl],
            AnyPrimaryUnit::Package(decl) => vec![decl],
            AnyPrimaryUnit::PackageInstance(decl) => vec![decl],
            AnyPrimaryUnit::Context(decl) => vec![decl],
        }
    }
}

impl ASTNode for ContextDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_context_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.items]
    }
}

impl ASTNode for ContextItem {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_context_item(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ContextItem::Use(clause) => vec![clause],
            ContextItem::Library(clause) => vec![clause],
            ContextItem::Context(clause) => vec![clause],
        }
    }
}

impl ASTNode for ContextReference {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_context_reference(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        as_node!(&self.name_list)
    }
}

impl ASTNode for LibraryClause {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_library_clause(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        as_node!(&self.name_list)
    }
}

impl ASTNode for UseClause {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_use_clause(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        as_node!(self.name_list)
    }
}

impl ASTNode for Ident {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_ident(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for PackageInstantiation {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_package_instantiation(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.package_name,
            &self.generic_map,
        ]
    }
}

impl ASTNode for AssociationElement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_association_element(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.formal, &self.actual]
    }
}

impl ASTNode for ActualPart {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_actual_part(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ActualPart::Expression(expr) => vec![expr],
            ActualPart::Open => vec![],
        }
    }
}

impl ASTNode for SelectedName {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_selected_name(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SelectedName::Designator(desi) => vec![desi],
            SelectedName::Selected(name, desi) => vec![name, desi],
        }
    }
}

impl ASTNode for Designator {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_designator(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for PackageDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_package_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.generic_clause,
            &self.decl,
        ]
    }
}

impl ASTNode for Declaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Declaration::Object(decl) => vec![decl],
            Declaration::File(decl) => vec![decl],
            Declaration::Type(decl) => vec![decl],
            Declaration::Component(decl) => vec![decl],
            Declaration::Attribute(decl) => vec![decl],
            Declaration::Alias(decl) => vec![decl],
            Declaration::SubprogramDeclaration(decl) => vec![decl],
            Declaration::SubprogramBody(decl) => vec![decl],
            Declaration::Use(decl) => vec![decl],
            Declaration::Package(decl) => vec![decl],
            Declaration::Configuration(decl) => vec![decl],
        }
    }
}

impl ASTNode for ConfigurationSpecification {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_configuration_specification(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.spec, &self.bind_ind, &self.vunit_bind_inds]
    }
}

impl ASTNode for VUnitBindingIndication {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_v_unit_binding_indication(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        as_node!(&self.vunit_list)
    }
}

impl ASTNode for BindingIndication {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_binding_indication(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.entity_aspect, &self.generic_map, &self.port_map]
    }
}

impl ASTNode for EntityAspect {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_entity_aspect(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            EntityAspect::Entity(name, id) => vec![name, id],
            EntityAspect::Configuration(config) => vec![config],
            EntityAspect::Open => vec![],
        }
    }
}

impl ASTNode for ComponentSpecification {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_component_specification(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.instantiation_list, &self.component_name]
    }
}

impl ASTNode for InstantiationList {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_instantiation_list(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            InstantiationList::Labels(idents) => as_node!(idents),
            InstantiationList::Others => vec![],
            InstantiationList::All => vec![],
        }
    }
}

impl ASTNode for SubprogramBody {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_subprogram_body(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.specification, &self.declarations, &self.statements]
    }
}

impl ASTNode for LabeledSequentialStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_labeled_sequential_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.label, &self.statement]
    }
}

impl ASTNode for SubprogramDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_subprogram_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SubprogramDeclaration::Procedure(proc) => vec![proc],
            SubprogramDeclaration::Function(func) => vec![func],
        }
    }
}

impl ASTNode for SequentialStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_sequential_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SequentialStatement::Wait(stmt) => vec![stmt],
            SequentialStatement::Assert(stmt) => vec![stmt],
            SequentialStatement::Report(stmt) => vec![stmt],
            SequentialStatement::VariableAssignment(stmt) => vec![stmt],
            SequentialStatement::SignalAssignment(stmt) => vec![stmt],
            SequentialStatement::SignalForceAssignment(stmt) => vec![stmt],
            SequentialStatement::SignalReleaseAssignment(stmt) => vec![stmt],
            SequentialStatement::ProcedureCall(stmt) => vec![stmt],
            SequentialStatement::If(stmt) => vec![stmt],
            SequentialStatement::Case(stmt) => vec![stmt],
            SequentialStatement::Loop(stmt) => vec![stmt],
            SequentialStatement::Next(stmt) => vec![stmt],
            SequentialStatement::Exit(stmt) => vec![stmt],
            SequentialStatement::Return(stmt) => vec![stmt],
            SequentialStatement::Null => vec![],
        }
    }
}

impl ASTNode for CaseStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_case_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.expression, &self.alternatives]
    }
}

impl ASTNode for ReturnStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_return_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match &self.expression {
            Some(expr) => vec![expr],
            None => vec![],
        }
    }
}

impl ASTNode for ExitStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_exit_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.loop_label, &self.condition]
    }
}

impl ASTNode for NextStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_next_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.loop_label, &self.condition]
    }
}

impl ASTNode for LoopStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_loop_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.iteration_scheme, &self.statements]
    }
}

impl ASTNode for IterationScheme {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_iteration_scheme(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            IterationScheme::While(scheme) => vec![scheme],
            IterationScheme::For(ident, range) => vec![ident, range],
        }
    }
}

impl ASTNode for DiscreteRange {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_discrete_range(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            DiscreteRange::Discrete(type_mark, range) => vec![type_mark, range],
            DiscreteRange::Range(range) => vec![range],
        }
    }
}

impl ASTNode for TypeMark {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_type_mark(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name]
    }
}

impl ASTNode for Range {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_range(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Range::Range(constraint) => vec![constraint],
            Range::Attribute(attr) => vec![attr.deref()],
        }
    }
}

impl ASTNode for RangeConstraint {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_range_constraint(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.left_expr, &self.right_expr]
    }
}

impl ASTNode for IfStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_if_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.conds]
    }
}

impl ASTNode for CallOrIndexed {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_call_or_indexed(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name, &self.parameters]
    }
}

impl ASTNode for SignalReleaseAssignment {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_signal_release_assignment(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.target]
    }
}

impl ASTNode for Target {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_target(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Target::Name(name) => vec![name],
            Target::Aggregate(aggr) => as_node!(aggr),
        }
    }
}

impl ASTNode for ElementAssociation {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_element_association(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ElementAssociation::Positional(expr) => vec![expr],
            ElementAssociation::Named(choices, expr) => vec![choices, expr],
        }
    }
}

impl ASTNode for SignalForceAssignment {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_signal_force_assignment(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.rhs]
    }
}

impl ASTNode for SignalAssignment {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_signal_assignment(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.target, &self.delay_mechanism, &self.rhs]
    }
}

impl ASTNode for DelayMechanism {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_delay_mechanism(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            DelayMechanism::Transport => vec![],
            DelayMechanism::Inertial { reject } => vec![reject],
        }
    }
}

impl ASTNode for Waveform {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_waveform(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Waveform::Elements(elements) => as_node!(elements),
            Waveform::Unaffected => vec![],
        }
    }
}

impl ASTNode for WaveformElement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_waveform_element(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.value, &self.after]
    }
}

impl ASTNode for VariableAssignment {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_variable_assignment(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.target, &self.rhs]
    }
}

impl ASTNode for ReportStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_report_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.severity, &self.report]
    }
}

impl ASTNode for AssertStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_assert_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.condition, &self.report, &self.severity]
    }
}

impl ASTNode for Choice {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_choice(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Choice::Expression(expr) => vec![expr],
            Choice::DiscreteRange(range) => vec![range],
            Choice::Others => vec![],
        }
    }
}

impl ASTNode for WaitStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_wait_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.sensitivity_clause,
            &self.condition_clause,
            &self.timeout_clause,
        ]
    }
}

impl ASTNode for FunctionSpecification {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_function_specification(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.parameter_list, &self.return_type]
    }
}

impl ASTNode for ProcedureSpecification {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_procedure_specification(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.designator, &self.parameter_list]
    }
}

impl ASTNode for SubprogramDesignator {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_subprogram_designator(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for AliasDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_alias_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.designator,
            &self.subtype_indication,
            &self.name,
            &self.signature,
        ]
    }
}

impl ASTNode for Attribute {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_attribute(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Attribute::Specification(spec) => vec![spec],
            Attribute::Declaration(decl) => vec![decl],
        }
    }
}

impl ASTNode for SubtypeIndication {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_subtype_indication(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.resolution, &self.type_mark, &self.constraint]
    }
}

impl ASTNode for AttributeSpecification {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_attribute_specification(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.entity_name, &self.expr]
    }
}

impl ASTNode for EntityName {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_entity_name(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            EntityName::Name(name) => vec![name],
            EntityName::All | EntityName::Others => vec![],
        }
    }
}

impl ASTNode for ResolutionIndication {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_resolution_indication(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ResolutionIndication::FunctionName(name) => vec![name],
            ResolutionIndication::ArrayElement(name) => vec![name],
            ResolutionIndication::Record(record) => as_node!(record),
            ResolutionIndication::Unresolved => vec![],
        }
    }
}

impl ASTNode for RecordElementResolution {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_record_element_resolution(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.resolution]
    }
}

impl ASTNode for SubtypeConstraint {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_subtype_constraint(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SubtypeConstraint::Range(range) => vec![range],
            SubtypeConstraint::Array(ranges, constraint) => vec![ranges, constraint],
            SubtypeConstraint::Record(constraints) => as_node!(constraints),
        }
    }
}

impl ASTNode for ElementConstraint {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_element_constraint(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.constraint]
    }
}

impl ASTNode for AttributeDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_attribute_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.type_mark]
    }
}

impl ASTNode for ComponentDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_component_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.generic_list, &self.port_list]
    }
}

impl ASTNode for TypeDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_type_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.def]
    }
}

impl ASTNode for TypeDefinition {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_type_definition(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            TypeDefinition::Enumeration(literals) => vec![literals],
            TypeDefinition::Numeric(range) => vec![range],
            TypeDefinition::Physical(decl) => vec![decl],
            TypeDefinition::Array(indices, indication) => vec![indices, indication],
            TypeDefinition::Record(record) => as_node!(record),
            TypeDefinition::Access(subtype) => vec![subtype],
            TypeDefinition::Incomplete(reference) => vec![reference],
            TypeDefinition::File(type_mark) => vec![type_mark],
            TypeDefinition::Protected(decl) => vec![decl],
            TypeDefinition::ProtectedBody(body) => vec![body],
            TypeDefinition::Subtype(subtype) => vec![subtype],
        }
    }
}

impl ASTNode for Reference {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_reference(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for ProtectedTypeBody {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_protected_type_body(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        as_node!(&self.decl)
    }
}

impl ASTNode for ProtectedTypeDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_protected_type_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        as_node!(&self.items)
    }
}

impl ASTNode for ProtectedTypeDeclarativeItem {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_protected_type_declarative_item(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ProtectedTypeDeclarativeItem::Subprogram(decl) => vec![decl],
        }
    }
}

impl ASTNode for ElementDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_element_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.subtype]
    }
}

impl ASTNode for ArrayIndex {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_array_index(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ArrayIndex::IndexSubtypeDefintion(type_mark) => vec![type_mark],
            ArrayIndex::Discrete(range) => vec![range],
        }
    }
}

impl ASTNode for PhysicalTypeDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_physical_type_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.range, &self.primary_unit, &self.secondary_units]
    }
}

impl ASTNode for PhysicalLiteral {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_physical_literal(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.value, &self.unit]
    }
}

impl ASTNode for EnumerationLiteral {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_enumeration_literal(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for FileDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_file_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.ident,
            &self.subtype_indication,
            &self.file_name,
            &self.open_info,
        ]
    }
}

impl ASTNode for ObjectDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_object_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.subtype_indication, &self.expression]
    }
}

impl ASTNode for InterfaceDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_interface_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            InterfaceDeclaration::Object(obj) => vec![obj],
            InterfaceDeclaration::File(obj) => vec![obj],
            InterfaceDeclaration::Type(obj) => vec![obj],
            InterfaceDeclaration::Subprogram(decl, default) => vec![decl, default],
            InterfaceDeclaration::Package(pkg) => vec![pkg],
        }
    }
}

impl ASTNode for InterfaceObjectDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_interface_object_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.ident,
            &self.subtype_indication,
            &self.expression,
        ]
    }
}

impl ASTNode for InterfaceFileDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_interface_file_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.subtype_indication]
    }
}

impl ASTNode for SubprogramDefault {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_subprogram_default(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SubprogramDefault::Name(name) => vec![name],
            SubprogramDefault::Box => vec![],
        }
    }
}

impl ASTNode for InterfacePackageDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_interface_package_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.ident, &self.package_name, &self.generic_map]
    }
}

impl ASTNode for InterfacePackageGenericMapAspect {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_interface_package_generic_map_aspect(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            InterfacePackageGenericMapAspect::Map(map) => as_node!(map),
            InterfacePackageGenericMapAspect::Box => vec![],
            InterfacePackageGenericMapAspect::Default => vec![],
        }
    }
}

impl ASTNode for ConfigurationDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_configuration_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.entity_name,
            &self.decl,
            &self.vunit_bind_inds,
            &self.block_config,
        ]
    }
}

impl ASTNode for ConfigurationDeclarativeItem {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_configuration_declarative_item(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ConfigurationDeclarativeItem::Use(clause) => vec![clause],
        }
    }
}

impl ASTNode for BlockConfiguration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_block_configuration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.block_spec, &self.use_clauses, &self.items]
    }
}

impl ASTNode for ConfigurationItem {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_configuration_item(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ConfigurationItem::Block(block) => vec![block],
            ConfigurationItem::Component(component) => vec![component],
        }
    }
}

impl ASTNode for ComponentConfiguration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_component_configuration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.spec,
            &self.bind_ind,
            &self.vunit_bind_inds,
            &self.block_config,
        ]
    }
}

impl ASTNode for EntityDeclaration {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_entity_declaration(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.generic_clause,
            &self.port_clause,
            &self.decl,
            &self.statements,
        ]
    }
}

impl ASTNode for AnySecondaryUnit {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_any_secondary_unit(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            AnySecondaryUnit::Architecture(arch) => vec![arch],
            AnySecondaryUnit::PackageBody(package) => vec![package],
        }
    }
}

impl ASTNode for LabeledConcurrentStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_labeled_concurrent_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.label, &self.statement]
    }
}

impl ASTNode for ConcurrentStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_concurrent_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match &self {
            ConcurrentStatement::ProcedureCall(stmt) => vec![stmt],
            ConcurrentStatement::Block(stmt) => vec![stmt],
            ConcurrentStatement::Process(stmt) => vec![stmt],
            ConcurrentStatement::Assert(stmt) => vec![stmt],
            ConcurrentStatement::Assignment(stmt) => vec![stmt],
            ConcurrentStatement::Instance(stmt) => vec![stmt],
            ConcurrentStatement::ForGenerate(stmt) => vec![stmt],
            ConcurrentStatement::IfGenerate(stmt) => vec![stmt],
            ConcurrentStatement::CaseGenerate(stmt) => vec![stmt],
        }
    }
}

impl ASTNode for CaseGenerateStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_case_generate_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.sels]
    }
}

impl ASTNode for IfGenerateStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_if_generate_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.conds]
    }
}

impl ASTNode for ForGenerateStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_for_generate_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.index_name, &self.discrete_range, &self.body]
    }
}

impl ASTNode for InstantiationStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_instantiation_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.unit, &self.generic_map, &self.port_map]
    }
}

impl ASTNode for GenerateBody {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_generate_body(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.alternative_label, &self.decl, &self.statements]
    }
}

impl ASTNode for InstantiatedUnit {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_instantiated_unit(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            InstantiatedUnit::Component(component) => vec![component],
            InstantiatedUnit::Entity(name, ident) => vec![name, ident],
            InstantiatedUnit::Configuration(config) => vec![config],
        }
    }
}

impl ASTNode for ConcurrentSignalAssignment {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_concurrent_signal_assignment(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.target, &self.delay_mechanism, &self.rhs]
    }
}

impl ASTNode for ConcurrentAssertStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_concurrent_assert_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.statement]
    }
}

impl ASTNode for ProcessStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_process_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.sensitivity_list, &self.decl, &self.statements]
    }
}

impl ASTNode for BlockStatement {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_block_statement(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.guard_condition,
            &self.header,
            &self.decl,
            &self.statements,
        ]
    }
}

impl ASTNode for BlockHeader {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_block_header(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.generic_map,
            &self.generic_map,
            &self.port_map,
            &self.port_map,
        ]
    }
}

impl ASTNode for ConcurrentProcedureCall {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_concurrent_procedure_call(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.call]
    }
}

impl ASTNode for PackageBody {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_package_body(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.context_clause, &self.ident, &self.decl]
    }
}

impl ASTNode for SensitivityList {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_sensitivity_list(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            SensitivityList::Names(names) => as_node!(names),
            SensitivityList::All => vec![],
        }
    }
}

impl ASTNode for ArchitectureBody {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_architecture_body(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![
            &self.context_clause,
            &self.ident,
            &self.entity_name,
            &self.decl,
            &self.statements,
        ]
    }
}

impl ASTNode for Expression {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_expression(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Expression::Binary(_, lhs, rhs) => vec![lhs, rhs],
            Expression::Unary(_, expr) => vec![expr],
            Expression::Aggregate(elements) => as_node!(elements),
            Expression::Qualified(qual) => vec![qual],
            Expression::Name(name) => vec![name],
            Expression::Literal(lit) => vec![lit],
            Expression::New(allocator) => vec![allocator],
        }
    }
}

impl ASTNode for QualifiedExpression {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_qualified_expression(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.type_mark, &self.expr]
    }
}

impl ASTNode for Allocator {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_allocator(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Allocator::Qualified(qual) => vec![qual],
            Allocator::Subtype(subtype) => vec![subtype],
        }
    }
}

impl ASTNode for AttributeDesignator {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_attribute_designator(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for Signature {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_signature(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Signature::Function(t1, t2) => vec![t1, t2],
            Signature::Procedure(proc) => vec![proc],
        }
    }
}

impl ASTNode for Name {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_name(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Name::Designator(desi) => vec![desi],
            Name::Selected(name, desi) => vec![name, desi],
            Name::SelectedAll(name) => vec![name],
            Name::Slice(name, range) => vec![name, range],
            Name::Attribute(attr) => vec![attr],
            Name::CallOrIndexed(coi) => vec![coi],
            Name::External(external) => vec![external],
        }
    }
}

impl ASTNode for ExternalName {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_external_name(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.path, &self.subtype]
    }
}

impl ASTNode for ExternalPath {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_external_path(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            ExternalPath::Package(name) => vec![name],
            ExternalPath::Absolute(name) => vec![name],
            ExternalPath::Relative(name, _) => vec![name],
        }
    }
}

impl ASTNode for AttributeName {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_attribute_name(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.name, &self.signature, &self.attr, &self.expr]
    }
}

impl ASTNode for AbstractLiteral {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_abstract_literal(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl ASTNode for Literal {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_literal(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        match self {
            Literal::String(_) | Literal::BitString(_) | Literal::Character(_) | Literal::Null => {
                vec![]
            }
            Literal::AbstractLiteral(lit) => vec![lit],
            Literal::Physical(phy) => vec![phy],
        }
    }
}

impl ASTNode for EntityTag {
    fn visit(&self, visitor: &dyn Visitor) -> VisitorResult {
        visitor.visit_entity_tag(self)
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![&self.designator, &self.signature]
    }
}
