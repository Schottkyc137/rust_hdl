// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::names::*;
use super::*;
use crate::ast::*;
use crate::data::*;
use crate::named_entity::{Signature, *};
use crate::{ast, named_entity, HasTokenSpan};
use analyze::*;
use fnv::FnvHashMap;
use itertools::Itertools;
use std::collections::hash_map::Entry;

impl Declaration {
    pub fn is_allowed_in_context(&self, parent: &AnyEntKind) -> bool {
        use Declaration::*;
        use ObjectClass::*;
        match parent {
            AnyEntKind::Design(Design::Architecture(..))
            | AnyEntKind::Concurrent(Some(Concurrent::Block | Concurrent::Generate)) => matches!(
                self,
                Object(ObjectDeclaration {
                    class: Constant | Signal | SharedVariable,
                    ..
                }) | File(_)
                    | Type(_)
                    | Component(_)
                    | Attribute(_)
                    | Alias(_)
                    | SubprogramDeclaration(_)
                    | SubprogramInstantiation(_)
                    | SubprogramBody(_)
                    | Use(_)
                    | Package(_)
                    | Configuration(_)
            ),
            AnyEntKind::Design(Design::Configuration) => {
                matches!(self, Use(_) | Attribute(ast::Attribute::Specification(_)))
            }
            AnyEntKind::Design(Design::Entity(..)) => matches!(
                self,
                Object(_)
                    | File(_)
                    | Type(_)
                    | Attribute(_)
                    | Alias(_)
                    | SubprogramDeclaration(_)
                    | SubprogramInstantiation(_)
                    | SubprogramBody(_)
                    | Use(_)
                    | Package(_)
            ),
            AnyEntKind::Design(Design::PackageBody | Design::UninstPackage(..))
            | AnyEntKind::Overloaded(
                Overloaded::SubprogramDecl(_)
                | Overloaded::Subprogram(_)
                | Overloaded::UninstSubprogramDecl(..)
                | Overloaded::UninstSubprogram(..),
            )
            | AnyEntKind::Concurrent(Some(Concurrent::Process))
            | AnyEntKind::Type(named_entity::Type::Protected(..)) => matches!(
                self,
                Object(ObjectDeclaration {
                    class: Constant | Variable | SharedVariable,
                    ..
                }) | File(_)
                    | Type(_)
                    | Attribute(_)
                    | Alias(_)
                    | SubprogramDeclaration(_)
                    | SubprogramInstantiation(_)
                    | SubprogramBody(_)
                    | Use(_)
                    | Package(_)
            ),
            AnyEntKind::Design(Design::Package(..)) => matches!(
                self,
                Object(_)
                    | File(_)
                    | Type(_)
                    | Component(_)
                    | Attribute(_)
                    | Alias(_)
                    | SubprogramDeclaration(_)
                    | SubprogramInstantiation(_)
                    | Use(_)
                    | Package(_)
            ),
            _ => {
                // AnyEntKind::Library is used in tests for a generic declarative region
                if !(cfg!(test) && matches!(parent, AnyEntKind::Library)) {
                    debug_assert!(false, "Parent should be a declarative region");
                }
                true
            }
        }
    }
}

impl<'a> AnalyzeContext<'a> {
    pub fn analyze_declarative_part(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        declarations: &mut [Declaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let mut incomplete_types: FnvHashMap<Symbol, (EntRef<'a>, SrcPos)> = FnvHashMap::default();

        for i in 0..declarations.len() {
            // Handle incomplete types

            let (decl, remaining) = declarations[i..].split_first_mut().unwrap();

            if !decl.is_allowed_in_context(parent.kind()) {
                diagnostics.error(
                    decl.get_pos(self.ctx),
                    format!("{} declaration not allowed here", decl.describe(),),
                )
            }

            match decl {
                Declaration::Type(type_decl) => match type_decl.def {
                    TypeDefinition::Incomplete(ref mut reference) => {
                        match incomplete_types.entry(type_decl.ident.name().clone()) {
                            Entry::Vacant(entry) => {
                                let full_definiton =
                                    find_full_type_definition(type_decl.ident.name(), remaining);

                                let decl_pos = match full_definiton {
                                    Some(full_decl) => full_decl.ident.pos(),
                                    None => {
                                        let mut error = Diagnostic::error(
                                            type_decl.ident.pos(),
                                            format!(
                                            "Missing full type declaration of incomplete type '{}'",
                                            type_decl.ident.name()
                                        ),
                                        );
                                        error.add_related(type_decl.ident.pos(), "The full type declaration shall occur immediately within the same declarative part");
                                        diagnostics.push(error);
                                        type_decl.ident.pos()
                                    }
                                };

                                let designator =
                                    Designator::Identifier(type_decl.ident.name().clone());

                                // Set incomplete type defintion to position of full declaration
                                let ent = self.arena.explicit(
                                    designator,
                                    parent,
                                    AnyEntKind::Type(Type::Incomplete),
                                    Some(decl_pos),
                                );
                                reference.set_unique_reference(ent);

                                entry.insert((ent, type_decl.ident.pos().clone()));
                                scope.add(ent, diagnostics);
                            }
                            Entry::Occupied(entry) => {
                                let (_, decl_pos) = entry.get();

                                diagnostics.push(Diagnostic::duplicate_error(
                                    &type_decl.ident,
                                    type_decl.ident.pos(),
                                    Some(decl_pos),
                                ));
                            }
                        }
                    }
                    _ => {
                        let incomplete_type = incomplete_types.get(type_decl.ident.name());
                        if let Some((incomplete_type, _)) = incomplete_type {
                            self.analyze_type_declaration(
                                scope,
                                parent,
                                type_decl,
                                Some(incomplete_type.id()),
                                diagnostics,
                            )?;
                        } else {
                            self.analyze_type_declaration(
                                scope,
                                parent,
                                type_decl,
                                None,
                                diagnostics,
                            )?;
                        }
                    }
                },
                _ => {
                    self.analyze_declaration(scope, parent, &mut declarations[i], diagnostics)?;
                }
            }
        }
        Ok(())
    }

    fn analyze_alias_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        alias: &mut AliasDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<EntRef<'a>> {
        let AliasDeclaration {
            designator,
            name,
            subtype_indication,
            signature,
            span: _,
        } = alias;

        let resolved_name = self.name_resolve(scope, &name.pos, &mut name.item, diagnostics);

        if let Some(ref mut subtype_indication) = subtype_indication {
            // Object alias
            self.analyze_subtype_indication(scope, subtype_indication, diagnostics)?;
        }

        let resolved_name = resolved_name?;

        let kind = {
            match resolved_name {
                ResolvedName::ObjectName(oname) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature("Alias", signature));
                    }
                    match oname.base {
                        ObjectBase::Object(base_object) => AnyEntKind::ObjectAlias {
                            base_object,
                            type_mark: oname.type_mark(),
                        },
                        ObjectBase::ObjectAlias(base_object, _) => AnyEntKind::ObjectAlias {
                            base_object,
                            type_mark: oname.type_mark(),
                        },
                        ObjectBase::ExternalName(class) => AnyEntKind::ExternalAlias {
                            class,
                            type_mark: oname.type_mark(),
                        },
                        ObjectBase::DeferredConstant(_) => {
                            // @TODO handle
                            return Err(EvalError::Unknown);
                        }
                    }
                }
                ResolvedName::Library(_)
                | ResolvedName::Design(_)
                | ResolvedName::Expression(_) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature("Alias", signature));
                    }
                    diagnostics.error(
                        &name.pos,
                        format!("{} cannot be aliased", resolved_name.describe_type()),
                    );
                    return Err(EvalError::Unknown);
                }
                ResolvedName::Type(typ) => {
                    if let Some(ref signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature("Alias", signature));
                    }
                    AnyEntKind::Type(Type::Alias(typ))
                }
                ResolvedName::Overloaded(des, overloaded) => {
                    if let Some(ref mut signature) = signature {
                        // TODO: Uninstantiated subprogram in aliases
                        match self.resolve_signature(scope, signature) {
                            Ok(signature_key) => {
                                if let Some(ent) =
                                    overloaded.get(&SubprogramKey::Normal(signature_key))
                                {
                                    if let Some(reference) = name.item.suffix_reference_mut() {
                                        reference.set_unique_reference(&ent);
                                    }
                                    AnyEntKind::Overloaded(Overloaded::Alias(ent))
                                } else {
                                    diagnostics.push(Diagnostic::no_overloaded_with_signature(
                                        &des.pos,
                                        &des.item,
                                        &overloaded,
                                    ));
                                    return Err(EvalError::Unknown);
                                }
                            }
                            Err(err) => {
                                err.add_to(diagnostics)?;
                                return Err(EvalError::Unknown);
                            }
                        }
                    } else {
                        diagnostics.push(Diagnostic::signature_required(name));
                        return Err(EvalError::Unknown);
                    }
                }
                ResolvedName::Final(_) => {
                    // @TODO some of these can probably be aliased
                    return Err(EvalError::Unknown);
                }
            }
        };

        Ok(designator.define(self.arena, parent, kind))
    }

    pub(crate) fn analyze_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        decl: &mut Declaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match decl {
            Declaration::Alias(alias) => {
                if let Some(ent) =
                    as_fatal(self.analyze_alias_declaration(scope, parent, alias, diagnostics))?
                {
                    scope.add(ent, diagnostics);

                    for implicit in ent.as_actual().implicits.iter() {
                        match OverloadedEnt::from_any(implicit) {
                            Some(implicit) => {
                                let impicit_alias = self.arena.implicit(
                                    ent,
                                    implicit.designator().clone(),
                                    AnyEntKind::Overloaded(Overloaded::Alias(implicit)),
                                    ent.decl_pos(),
                                );
                                scope.add(impicit_alias, diagnostics);
                            }
                            None => {
                                eprintln!(
                                    "Expect implicit declaration to be overloaded, got: {}",
                                    implicit.describe()
                                )
                            }
                        }
                    }
                }
            }
            Declaration::Object(ref mut object_decl) => {
                let subtype = self.resolve_subtype_indication(
                    scope,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                );

                if let Some(ref mut expr) = object_decl.expression {
                    if let Ok(ref subtype) = subtype {
                        self.expr_pos_with_ttyp(
                            scope,
                            subtype.type_mark(),
                            &expr.pos,
                            &mut expr.item,
                            diagnostics,
                        )?;
                    } else {
                        self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                    }
                }

                match subtype {
                    Ok(subtype) => {
                        let kind = if object_decl.class == ObjectClass::Constant
                            && object_decl.expression.is_none()
                        {
                            AnyEntKind::DeferredConstant(subtype)
                        } else {
                            AnyEntKind::Object(Object {
                                class: object_decl.class,
                                iface: None,
                                has_default: object_decl.expression.is_some(),
                                subtype,
                            })
                        };

                        let declared_by = if object_decl.class == ObjectClass::Constant
                            && object_decl.expression.is_some()
                        {
                            self.find_deferred_constant_declaration(
                                scope,
                                &object_decl.ident.tree.item,
                            )
                        } else {
                            None
                        };

                        let object_ent = self.arena.alloc(
                            object_decl.ident.tree.item.clone().into(),
                            Some(parent),
                            if let Some(declared_by) = declared_by {
                                Related::DeclaredBy(declared_by)
                            } else {
                                Related::None
                            },
                            kind,
                            Some(object_decl.ident.tree.pos().clone()),
                        );
                        object_decl.ident.decl = Some(object_ent.id());

                        scope.add(object_ent, diagnostics);
                    }
                    Err(err) => err.add_to(diagnostics)?,
                }
            }
            Declaration::File(ref mut file) => {
                let FileDeclaration {
                    ident,
                    subtype_indication,
                    open_info,
                    file_name,
                    span: _,
                } = file;

                let subtype =
                    match self.resolve_subtype_indication(scope, subtype_indication, diagnostics) {
                        Ok(subtype) => Some(subtype),
                        Err(err) => {
                            err.add_to(diagnostics)?;
                            None
                        }
                    };

                if let Some(ref mut expr) = open_info {
                    self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                }
                if let Some(ref mut expr) = file_name {
                    self.expr_unknown_ttyp(scope, expr, diagnostics)?;
                }

                if let Some(subtype) = subtype {
                    scope.add(
                        self.arena.define(ident, parent, AnyEntKind::File(subtype)),
                        diagnostics,
                    );
                }
            }
            Declaration::Component(ref mut component) => {
                let nested = scope.nested();
                let ent = self.arena.define(
                    &mut component.ident,
                    parent,
                    AnyEntKind::Component(Region::default()),
                );
                self.analyze_interface_list(
                    &nested,
                    ent,
                    &mut component.generic_list,
                    diagnostics,
                )?;
                self.analyze_interface_list(&nested, ent, &mut component.port_list, diagnostics)?;

                let kind = AnyEntKind::Component(nested.into_region());
                unsafe {
                    ent.set_kind(kind);
                }

                scope.add(ent, diagnostics);
            }
            Declaration::Attribute(ref mut attr) => match attr {
                Attribute::Declaration(ref mut attr_decl) => {
                    match self.resolve_type_mark(scope, &mut attr_decl.type_mark) {
                        Ok(typ) => {
                            scope.add(
                                self.arena.define(
                                    &mut attr_decl.ident,
                                    parent,
                                    AnyEntKind::Attribute(typ),
                                ),
                                diagnostics,
                            );
                        }
                        Err(err) => {
                            err.add_to(diagnostics)?;
                        }
                    }
                }
                Attribute::Specification(ref mut attr_spec) => {
                    self.attribute_specification(scope, parent, attr_spec, diagnostics)?;
                }
            },
            Declaration::SubprogramBody(ref mut body) => {
                let (subpgm_region, subpgm_ent) = match self.subprogram_specification(
                    scope,
                    parent,
                    &mut body.specification,
                    Overloaded::Subprogram,
                    diagnostics,
                ) {
                    Ok(r) => r,
                    Err(err) => {
                        diagnostics.push(err.into_non_fatal()?);
                        return Ok(());
                    }
                };

                scope.add(subpgm_ent.into(), diagnostics);

                self.define_labels_for_sequential_part(
                    &subpgm_region,
                    subpgm_ent.into(),
                    &mut body.statements,
                    diagnostics,
                )?;
                self.analyze_declarative_part(
                    &subpgm_region,
                    subpgm_ent.into(),
                    &mut body.declarations,
                    diagnostics,
                )?;

                self.analyze_sequential_part(
                    &subpgm_region,
                    subpgm_ent.into(),
                    &mut body.statements,
                    diagnostics,
                )?;
            }
            Declaration::SubprogramDeclaration(ref mut subdecl) => {
                match self.subprogram_specification(
                    scope,
                    parent,
                    &mut subdecl.specification,
                    Overloaded::SubprogramDecl,
                    diagnostics,
                ) {
                    Ok((_, ent)) => {
                        scope.add(ent.into(), diagnostics);
                    }
                    Err(err) => {
                        diagnostics.push(err.into_non_fatal()?);
                        return Ok(());
                    }
                }
            }
            Declaration::SubprogramInstantiation(ref mut instance) => {
                let subpgm_ent = self.arena.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Overloaded(Overloaded::Subprogram(Signature::new(
                        FormalRegion::new_params(),
                        None,
                    ))),
                );
                let referenced_name = &mut instance.subprogram_name;
                if let Some(name) = as_fatal(self.name_resolve(
                    scope,
                    &referenced_name.pos,
                    &mut referenced_name.item,
                    diagnostics,
                ))? {
                    match self.generic_subprogram_instance(
                        scope,
                        &subpgm_ent,
                        &name,
                        instance,
                        diagnostics,
                    ) {
                        Ok(signature) => {
                            unsafe {
                                subpgm_ent.set_kind(AnyEntKind::Overloaded(Overloaded::Subprogram(
                                    signature,
                                )))
                            }
                            scope.add(subpgm_ent, diagnostics)
                        }
                        Err(err) => err.add_to(diagnostics)?,
                    }
                }
            }
            Declaration::Use(ref mut use_clause) => {
                self.analyze_use_clause(scope, use_clause, diagnostics)?;
            }

            Declaration::Package(ref mut instance) => {
                let ent = self.arena.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Design(Design::PackageInstance(Region::default())),
                );

                if let Some(pkg_region) =
                    as_fatal(self.generic_package_instance(scope, ent, instance, diagnostics))?
                {
                    let kind = AnyEntKind::Design(Design::PackageInstance(pkg_region));
                    unsafe {
                        ent.set_kind(kind);
                    }
                    scope.add(ent, diagnostics);
                }
            }
            Declaration::Configuration(..) => {}
            Declaration::Type(..) => unreachable!("Handled elsewhere"),
        };

        Ok(())
    }

    /// Analyze a generic subprogram instance, i.e.,
    /// ```vhdl
    /// procedure my_proc is new my_proc generic map (T => std_logic);
    /// ```
    ///
    /// # Arguments
    ///
    /// * `scope` - The scope that this instance was declared in
    /// * `inst_subprogram_ent` - A reference to the instantiated subprogram entity.
    ///     Used to set the parent reference of the signature
    /// * `uninst_name` - The [ResolvedName] of the uninstantiated subprogram
    /// * `instance` - A reference to the AST element of the subprogram instantiation
    /// * `diagnostics` - The diagnostics handler
    ///
    /// # Returns
    /// The signature after applying the optional map aspect of the uninstantiated subprogram
    fn generic_subprogram_instance(
        &self,
        scope: &Scope<'a>,
        inst_subprogram_ent: &EntRef<'a>,
        uninst_name: &ResolvedName<'a>,
        instance: &mut SubprogramInstantiation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Signature> {
        let uninstantiated_subprogram =
            self.resolve_uninstantiated_subprogram(scope, uninst_name, instance)?;
        self.check_instantiated_subprogram_kind_matches_declared(
            &uninstantiated_subprogram,
            instance,
            diagnostics,
        );
        instance
            .subprogram_name
            .item
            .set_unique_reference(&uninstantiated_subprogram);
        let region = match uninstantiated_subprogram.kind() {
            Overloaded::UninstSubprogramDecl(_, region) => region,
            Overloaded::UninstSubprogram(_, region) => region,
            _ => unreachable!(),
        };

        match as_fatal(self.generic_instance(
            inst_subprogram_ent,
            scope,
            &instance.ident.tree.pos,
            region,
            &mut instance.generic_map,
            diagnostics,
        ))? {
            None => Ok(uninstantiated_subprogram.signature().clone()),
            Some((_, mapping)) => {
                match self.map_signature(
                    Some(inst_subprogram_ent),
                    &mapping,
                    uninstantiated_subprogram.signature(),
                ) {
                    Ok(signature) => Ok(signature),
                    Err(err) => {
                        let mut diag = Diagnostic::error(&instance.ident.tree.pos, err);
                        if let Some(pos) = uninstantiated_subprogram.decl_pos() {
                            diag.add_related(pos, "When instantiating this declaration");
                        }
                        Err(AnalysisError::NotFatal(diag))
                    }
                }
            }
        }
    }

    /// Given a `ResolvedName` and the subprogram instantiation,
    /// find the uninstantiated subprogram that the resolved name references.
    /// Return that resolved subprogram, if it exists, else return an `Err`
    fn resolve_uninstantiated_subprogram(
        &self,
        scope: &Scope<'a>,
        name: &ResolvedName<'a>,
        instantiation: &mut SubprogramInstantiation,
    ) -> AnalysisResult<OverloadedEnt<'a>> {
        let signature_key = match &mut instantiation.signature {
            None => None,
            Some(ref mut signature) => Some((
                self.resolve_signature(scope, signature)?,
                signature.pos.clone(),
            )),
        };
        let overloaded_ent = match name {
            ResolvedName::Overloaded(_, overloaded) => {
                let choices = overloaded
                    .entities()
                    .filter(|ent| ent.is_uninst_subprogram())
                    .collect_vec();
                if choices.is_empty() {
                    Err(AnalysisError::NotFatal(Diagnostic::error(
                        &instantiation.ident.tree.pos,
                        format!(
                            "{} does not denote an uninstantiated subprogram",
                            name.describe()
                        ),
                    )))
                } else if choices.len() == 1 {
                    // There is only one possible candidate
                    let ent = choices[0];
                    // If the instantiated program has a signature, check that it matches
                    // that of the uninstantiated subprogram
                    if let Some((key, pos)) = signature_key {
                        match overloaded.get(&SubprogramKey::Uninstantiated(key)) {
                            None => Err(AnalysisError::NotFatal(Diagnostic::error(
                                pos.clone(),
                                format!(
                                    "Signature does not match the the signature of {}",
                                    ent.describe()
                                ),
                            ))),
                            Some(_) => Ok(ent),
                        }
                    } else {
                        Ok(ent)
                    }
                } else if let Some((key, _)) = signature_key {
                    // There are multiple candidates
                    // but there is a signature that we can try to resolve
                    if let Some(resolved_ent) =
                        overloaded.get(&SubprogramKey::Uninstantiated(key.clone()))
                    {
                        Ok(resolved_ent)
                    } else {
                        Err(AnalysisError::NotFatal(Diagnostic::error(
                            &instantiation.subprogram_name.pos,
                            format!(
                                "No uninstantiated subprogram exists with signature {}",
                                key.describe()
                            ),
                        )))
                    }
                } else {
                    // There are multiple candidates
                    // and there is no signature to resolve
                    let mut err = Diagnostic::error(
                        &instantiation.subprogram_name.pos,
                        format!("Ambiguous instantiation of '{}'", overloaded.designator()),
                    );
                    for ent in choices {
                        if let Some(pos) = &ent.decl_pos {
                            err.add_related(pos.clone(), format!("Might be {}", ent.describe()))
                        }
                    }
                    Err(AnalysisError::NotFatal(err))
                }
            }
            _ => Err(AnalysisError::NotFatal(Diagnostic::error(
                &instantiation.subprogram_name.pos,
                format!(
                    "{} does not denote an uninstantiated subprogram",
                    name.describe()
                ),
            ))),
        }?;
        if overloaded_ent.is_uninst_subprogram() {
            Ok(overloaded_ent)
        } else {
            Err(AnalysisError::NotFatal(Diagnostic::error(
                &instantiation.subprogram_name.pos,
                format!("{} cannot be instantiated", overloaded_ent.describe()),
            )))
        }
    }

    /// Checks that an instantiated subprogram kind matches the declared subprogram.
    /// For instance, when a subprogram was instantiated using
    /// ```vhdl
    /// function my_func is new proc;
    /// ```
    /// where proc is
    /// ```vhdl
    /// procedure proc is
    /// ...
    /// ```
    ///
    /// This function will push an appropriate diagnostic.
    fn check_instantiated_subprogram_kind_matches_declared(
        &self,
        ent: &OverloadedEnt,
        instance: &SubprogramInstantiation,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        let err_msg = if ent.is_function() && instance.kind != SubprogramKind::Function {
            Some("Instantiating function as procedure")
        } else if ent.is_procedure() && instance.kind != SubprogramKind::Procedure {
            Some("Instantiating procedure as function")
        } else {
            None
        };
        if let Some(msg) = err_msg {
            let mut err = Diagnostic::error(self.ctx.get_pos(instance.get_start_token()), msg);
            if let Some(pos) = ent.decl_pos() {
                err.add_related(pos, format!("{} declared here", ent.describe()));
            }
            diagnostics.push(err)
        }
    }

    fn find_subpgm_specification(
        &self,
        scope: &Scope<'a>,
        decl: &SubprogramSpecification,
        signature: &Signature,
    ) -> Option<OverloadedEnt<'a>> {
        let des = decl.subpgm_designator().item.clone().into_designator();

        if let Some(NamedEntities::Overloaded(overloaded)) = scope.lookup_immediate(&des) {
            let ent = overloaded.get(&SubprogramKey::Normal(signature.key()))?;

            if ent.is_subprogram_decl() {
                return Some(ent);
            }
        }
        None
    }

    fn find_uninst_subpgm_specification(
        &self,
        scope: &Scope<'a>,
        decl: &SubprogramSpecification,
        signature: &Signature,
    ) -> Option<OverloadedEnt<'a>> {
        let des = decl.subpgm_designator().item.clone().into_designator();

        if let Some(NamedEntities::Overloaded(overloaded)) = scope.lookup_immediate(&des) {
            // Note: This does not work in common circumstances with a generic type parameter
            // since the parameters of the declared subprogram and the subprogram with body
            // point to two different type-ID's. For example:
            // function foo generic (type F);
            //                            ^-- F has EntityId X
            // function foo generic (type F) return F is ... end function foo;
            //                            ^-- F has EntityId Y
            // A future improvement must take this fact into account.
            let ent = overloaded.get(&SubprogramKey::Uninstantiated(signature.key()))?;

            if ent.is_uninst_subprogram_decl() {
                return Some(ent);
            }
        }
        None
    }

    fn find_deferred_constant_declaration(
        &self,
        scope: &Scope<'a>,
        ident: &Symbol,
    ) -> Option<EntRef<'a>> {
        if let Some(NamedEntities::Single(ent)) = scope.lookup_immediate(&ident.into()) {
            if ent.kind().is_deferred_constant() {
                return Some(ent);
            }
        }
        None
    }

    fn attribute_specification(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        attr_spec: &mut AttributeSpecification,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        let AttributeSpecification {
            ident,
            entity_name,
            entity_class,
            expr,
            span: _,
        } = attr_spec;

        let attr_ent = match scope.lookup(
            &ident.item.pos,
            &Designator::Identifier(ident.item.name().clone()),
        ) {
            Ok(NamedEntities::Single(ent)) => {
                ident.set_unique_reference(ent);
                if let Some(attr_ent) = AttributeEnt::from_any(ent) {
                    self.expr_pos_with_ttyp(
                        scope,
                        attr_ent.typ(),
                        &expr.pos,
                        &mut expr.item,
                        diagnostics,
                    )?;
                    attr_ent
                } else {
                    diagnostics.error(
                        &ident.item.pos,
                        format!("{} is not an attribute", ent.describe()),
                    );
                    return Ok(());
                }
            }
            Ok(NamedEntities::Overloaded(_)) => {
                diagnostics.error(
                    &ident.item.pos,
                    format!("Overloaded name '{}' is not an attribute", ident.item),
                );
                return Ok(());
            }
            Err(err) => {
                diagnostics.push(err);
                return Ok(());
            }
        };

        if let EntityName::Name(EntityTag {
            designator,
            signature,
        }) = entity_name
        {
            let ent: EntRef = match scope.lookup(&designator.pos, &designator.item.item) {
                Ok(NamedEntities::Single(ent)) => {
                    designator.set_unique_reference(ent);

                    if let Some(signature) = signature {
                        diagnostics.push(Diagnostic::should_not_have_signature(
                            "Attribute specification",
                            &signature.pos,
                        ));
                    }
                    ent
                }
                Ok(NamedEntities::Overloaded(overloaded)) => {
                    if let Some(signature) = signature {
                        match self.resolve_signature(scope, signature) {
                            Ok(signature_key) => {
                                if let Some(ent) =
                                    overloaded.get(&SubprogramKey::Normal(signature_key))
                                {
                                    designator.set_unique_reference(&ent);
                                    ent.into()
                                } else {
                                    diagnostics.push(Diagnostic::no_overloaded_with_signature(
                                        &designator.pos,
                                        &designator.item.item,
                                        &overloaded,
                                    ));
                                    return Ok(());
                                }
                            }
                            Err(err) => {
                                err.add_to(diagnostics)?;
                                return Ok(());
                            }
                        }
                    } else if let Some(ent) = overloaded.as_unique() {
                        designator.set_unique_reference(ent);
                        ent
                    } else {
                        diagnostics.push(Diagnostic::signature_required(designator));
                        return Ok(());
                    }
                }
                Err(err) => {
                    diagnostics.push(err);
                    return Ok(());
                }
            };

            // Attributes affect the underlying entity and cannot be set directly on aliases
            let ent = ent.as_actual();

            match entity_class {
                EntityClass::Architecture
                | EntityClass::Entity
                | EntityClass::Package
                | EntityClass::Configuration => {
                    if ent != parent {
                        diagnostics.push(Diagnostic::error(
                            designator,
                            "Attribute specification must be in the immediate declarative part",
                        ));
                        return Ok(());
                    }
                }
                EntityClass::Signal
                | EntityClass::Variable
                | EntityClass::Procedure
                | EntityClass::Function
                | EntityClass::Component
                | EntityClass::Constant
                | EntityClass::Type
                | EntityClass::Subtype
                | EntityClass::Literal
                | EntityClass::Units
                | EntityClass::File
                | EntityClass::Label => {
                    if ent.parent != Some(parent) {
                        diagnostics.push(Diagnostic::error(
                            designator,
                            "Attribute specification must be in the immediate declarative part",
                        ));
                        return Ok(());
                    }
                }
            }

            if Some(*entity_class) != get_entity_class(ent) {
                diagnostics.push(Diagnostic::error(
                    designator,
                    format!("{} is not of class {}", ent.describe(), entity_class),
                ));
                return Ok(());
            }

            let res = unsafe { self.arena.add_attr(ent.id(), &designator.pos, attr_ent) };

            if let Err(diagnostic) = res {
                diagnostics.push(diagnostic);
            }
        }

        Ok(())
    }

    pub(crate) fn analyze_type_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        type_decl: &mut TypeDeclaration,
        // Is the full type declaration of an incomplete type
        // Overwrite id when defining full type
        overwrite_id: Option<EntityId>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match type_decl.def {
            TypeDefinition::Enumeration(ref mut enumeration) => {
                let enum_type = TypeEnt::define_with_opt_id(
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Enum(
                        enumeration
                            .iter()
                            .map(|literal| literal.tree.item.clone().into_designator())
                            .collect(),
                    ),
                );

                let signature =
                    Signature::new(FormalRegion::new(InterfaceType::Parameter), Some(enum_type));

                for literal in enumeration.iter_mut() {
                    let literal_ent = self.arena.explicit(
                        literal.tree.item.clone().into_designator(),
                        enum_type.into(),
                        AnyEntKind::Overloaded(Overloaded::EnumLiteral(signature.clone())),
                        Some(&literal.tree.pos),
                    );
                    literal.decl = Some(literal_ent.id());

                    unsafe {
                        self.arena.add_implicit(enum_type.id(), literal_ent);
                    }

                    scope.add(literal_ent, diagnostics);
                }

                scope.add(enum_type.into(), diagnostics);

                for ent in self.enum_implicits(enum_type, self.has_matching_op(enum_type)) {
                    unsafe {
                        self.arena.add_implicit(enum_type.id(), ent);
                    }

                    scope.add(ent, diagnostics);
                }
            }
            TypeDefinition::ProtectedBody(ref mut body) => {
                match scope.lookup_immediate(&type_decl.ident.tree.item.clone().into()) {
                    Some(visible) => {
                        let is_ok = match visible.clone().into_non_overloaded() {
                            Ok(ent) => {
                                if let AnyEntKind::Type(Type::Protected(ptype_region, is_body)) =
                                    ent.kind()
                                {
                                    if *is_body {
                                        if let Some(prev_pos) = ent.decl_pos() {
                                            diagnostics.push(Diagnostic::duplicate_error(
                                                &type_decl.ident.tree,
                                                &type_decl.ident.tree.pos,
                                                Some(prev_pos),
                                            ))
                                        }
                                    } else {
                                        let ptype_body: &'a AnyEnt = TypeEnt::define_with_opt_id(
                                            self.arena,
                                            overwrite_id,
                                            &mut type_decl.ident,
                                            parent,
                                            Some(ent),
                                            Type::Protected(Region::default(), true),
                                        )
                                        .into();

                                        let region = Scope::extend(ptype_region, Some(scope));
                                        self.analyze_declarative_part(
                                            &region,
                                            ptype_body,
                                            &mut body.decl,
                                            diagnostics,
                                        )?;

                                        let kind = Type::Protected(region.into_region(), true);
                                        unsafe {
                                            ptype_body.set_kind(AnyEntKind::Type(kind));
                                        }

                                        scope.add(ptype_body, diagnostics);
                                    }

                                    true
                                } else {
                                    false
                                }
                            }
                            _ => false,
                        };

                        if !is_ok {
                            diagnostics.push(Diagnostic::error(
                                type_decl.ident.pos(),
                                format!("'{}' is not a protected type", &type_decl.ident),
                            ));
                        }
                    }
                    None => {
                        diagnostics.push(Diagnostic::error(
                            type_decl.ident.pos(),
                            format!("No declaration of protected type '{}'", &type_decl.ident),
                        ));
                    }
                };
            }
            TypeDefinition::Protected(ref mut prot_decl) => {
                // Protected type name is visible inside its declarative region
                // This will be overwritten later when the protected type region is finished
                let ptype: &'a AnyEnt = TypeEnt::define_with_opt_id(
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Protected(Region::default(), false),
                )
                .into();

                scope.add(ptype, diagnostics);

                let region = scope.nested();
                for item in prot_decl.items.iter_mut() {
                    match item {
                        ProtectedTypeDeclarativeItem::Subprogram(ref mut subprogram) => {
                            match self.subprogram_specification(
                                scope,
                                ptype,
                                &mut subprogram.specification,
                                Overloaded::SubprogramDecl,
                                diagnostics,
                            ) {
                                Ok((_, ent)) => {
                                    region.add(ent.into(), diagnostics);
                                }
                                Err(err) => {
                                    diagnostics.push(err.into_non_fatal()?);
                                    return Ok(());
                                }
                            }
                        }
                    }
                }

                // This is safe since we are in a single thread and no other reference can exist yes
                // Also the region is stored inside an Arc which cannot move
                {
                    let AnyEntKind::Type(Type::Protected(region_ptr, _)) = ptype.kind() else {
                        unreachable!();
                    };

                    let region_ptr = unsafe {
                        let region_ptr = region_ptr as *const Region;
                        let region_ptr = region_ptr as *mut Region;
                        &mut *region_ptr as &mut Region
                    };
                    *region_ptr = region.into_region();
                }
            }
            TypeDefinition::Record(ref mut element_decls) => {
                let type_ent = TypeEnt::define_with_opt_id(
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Record(RecordRegion::default()),
                );

                let mut elems = RecordRegion::default();
                let mut region = Region::default();
                for elem_decl in element_decls.iter_mut() {
                    let subtype =
                        self.resolve_subtype_indication(scope, &mut elem_decl.subtype, diagnostics);
                    match subtype {
                        Ok(subtype) => {
                            let elem = self.arena.define(
                                &mut elem_decl.ident,
                                type_ent.into(),
                                AnyEntKind::ElementDeclaration(subtype),
                            );
                            region.add(elem, diagnostics);
                            elems.add(elem);
                        }
                        Err(err) => {
                            err.add_to(diagnostics)?;
                        }
                    }
                }
                region.close(diagnostics);

                unsafe {
                    let kind = AnyEntKind::Type(Type::Record(elems));
                    type_ent.set_kind(kind)
                }

                scope.add(type_ent.into(), diagnostics);

                for ent in self.record_implicits(type_ent) {
                    unsafe {
                        self.arena.add_implicit(type_ent.id(), ent);
                    }
                    scope.add(ent, diagnostics);
                }
            }
            TypeDefinition::Access(ref mut subtype_indication) => {
                let subtype =
                    self.resolve_subtype_indication(scope, subtype_indication, diagnostics);
                match subtype {
                    Ok(subtype) => {
                        let type_ent = TypeEnt::define_with_opt_id(
                            self.arena,
                            overwrite_id,
                            &mut type_decl.ident,
                            parent,
                            None,
                            Type::Access(subtype),
                        );

                        scope.add(type_ent.into(), diagnostics);

                        for ent in self.access_implicits(type_ent) {
                            unsafe {
                                self.arena.add_implicit(type_ent.id(), ent);
                            }
                            scope.add(ent, diagnostics);
                        }
                    }
                    Err(err) => err.add_to(diagnostics)?,
                }
            }
            TypeDefinition::Array(ref mut array_indexes, ref mut subtype_indication) => {
                let mut indexes: Vec<Option<BaseType>> = Vec::with_capacity(array_indexes.len());
                for index in array_indexes.iter_mut() {
                    indexes.push(as_fatal(self.analyze_array_index(
                        scope,
                        index,
                        diagnostics,
                    ))?);
                }

                let elem_type =
                    match self.resolve_subtype_indication(scope, subtype_indication, diagnostics) {
                        Ok(subtype) => subtype.type_mark().to_owned(),
                        Err(err) => {
                            err.add_to(diagnostics)?;
                            return Ok(());
                        }
                    };

                let is_1d = indexes.len() == 1;
                let array_ent = TypeEnt::define_with_opt_id(
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Array { indexes, elem_type },
                );

                scope.add(array_ent.into(), diagnostics);

                for ent in self.array_implicits(array_ent, is_1d && self.has_matching_op(elem_type))
                {
                    unsafe {
                        self.arena.add_implicit(array_ent.id(), ent);
                    }
                    scope.add(ent, diagnostics);
                }
            }
            TypeDefinition::Subtype(ref mut subtype_indication) => {
                match self.resolve_subtype_indication(scope, subtype_indication, diagnostics) {
                    Ok(subtype) => {
                        let type_ent = TypeEnt::define_with_opt_id(
                            self.arena,
                            overwrite_id,
                            &mut type_decl.ident,
                            parent,
                            None,
                            Type::Subtype(subtype),
                        );
                        scope.add(type_ent.into(), diagnostics);
                    }
                    Err(err) => {
                        err.add_to(diagnostics)?;
                    }
                }
            }
            TypeDefinition::Physical(ref mut physical) => {
                self.range_with_ttyp(
                    scope,
                    self.universal_integer().into(),
                    &mut physical.range,
                    diagnostics,
                )?;

                let phys_type = TypeEnt::define_with_opt_id(
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::Physical,
                );
                scope.add(phys_type.into(), diagnostics);

                let primary = self.arena.define(
                    &mut physical.primary_unit,
                    parent,
                    AnyEntKind::PhysicalLiteral(phys_type),
                );

                unsafe {
                    self.arena.add_implicit(phys_type.id(), primary);
                }
                scope.add(primary, diagnostics);

                for (secondary_unit_name, value) in physical.secondary_units.iter_mut() {
                    match self.resolve_physical_unit(scope, &mut value.unit) {
                        Ok(secondary_unit_type) => {
                            if secondary_unit_type.base_type() != phys_type {
                                diagnostics.error(
                                    &value.unit.item.pos,
                                    format!(
                                        "Physical unit of type '{}' does not match {}",
                                        secondary_unit_type.designator(),
                                        phys_type.describe()
                                    ),
                                )
                            }
                        }
                        Err(err) => diagnostics.push(err),
                    }

                    let secondary_unit = self.arena.define(
                        secondary_unit_name,
                        parent,
                        AnyEntKind::PhysicalLiteral(phys_type),
                    );
                    unsafe {
                        self.arena.add_implicit(phys_type.id(), secondary_unit);
                    }
                    scope.add(secondary_unit, diagnostics)
                }

                for ent in self.physical_implicits(phys_type) {
                    unsafe {
                        self.arena.add_implicit(phys_type.id(), ent);
                    }
                    scope.add(ent, diagnostics);
                }
            }
            TypeDefinition::Incomplete(..) => {
                unreachable!("Handled elsewhere");
            }

            TypeDefinition::Numeric(ref mut range) => {
                self.range_unknown_typ(scope, range, diagnostics)?;

                let universal_type = if let Some(range_typ) =
                    as_fatal(self.range_type(scope, range, diagnostics))?
                {
                    if range_typ.is_any_integer() {
                        UniversalType::Integer
                    } else if range_typ.is_any_real() {
                        UniversalType::Real
                    } else {
                        diagnostics.error(&range.pos(), "Expected real or integer range");
                        return Ok(());
                    }
                } else {
                    return Ok(());
                };

                let type_ent = TypeEnt::define_with_opt_id(
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    match universal_type {
                        UniversalType::Integer => Type::Integer,
                        UniversalType::Real => Type::Real,
                    },
                );
                scope.add(type_ent.into(), diagnostics);

                for ent in self.numeric_implicits(universal_type, type_ent) {
                    unsafe {
                        self.arena.add_implicit(type_ent.id(), ent);
                    }
                    scope.add(ent, diagnostics);
                }
            }

            TypeDefinition::File(ref mut type_mark) => {
                let file_type = TypeEnt::define_with_opt_id(
                    self.arena,
                    overwrite_id,
                    &mut type_decl.ident,
                    parent,
                    None,
                    Type::File,
                );

                match self.resolve_type_mark(scope, type_mark) {
                    Ok(type_mark) => {
                        for ent in self.create_implicit_file_type_subprograms(file_type, type_mark)
                        {
                            unsafe {
                                self.arena.add_implicit(file_type.id(), ent);
                            }
                            scope.add(ent, diagnostics);
                        }
                    }
                    Err(err) => {
                        err.add_to(diagnostics)?;
                    }
                }

                scope.add(file_type.into(), diagnostics);
            }
        }

        Ok(())
    }

    /// The matching operators such as ?= are defined for 1d arrays of bit and std_ulogic element type
    fn has_matching_op(&self, typ: TypeEnt<'a>) -> bool {
        if self.is_std_logic_1164 {
            // Within the std_logic_1164 we do not have efficient access to the types
            typ.designator() == &Designator::Identifier(self.root.symbol_utf8("std_ulogic"))
        } else {
            if let Some(ref standard_types) = self.root.standard_types {
                if typ.id() == standard_types.bit {
                    return true;
                }
            }

            if let Some(id) = self.root.std_ulogic {
                if typ.id() == id {
                    return true;
                }
            }

            false
        }
    }

    pub fn resolve_signature(
        &self,
        scope: &Scope<'a>,
        signature: &mut WithPos<ast::Signature>,
    ) -> AnalysisResult<SignatureKey> {
        let (args, return_type) = match &mut signature.item {
            ast::Signature::Function(ref mut args, ref mut ret) => {
                let args: Vec<_> = args
                    .iter_mut()
                    .map(|arg| self.resolve_type_mark(scope, arg))
                    .collect();
                let return_type = self.resolve_type_mark(scope, ret);
                (args, Some(return_type))
            }
            ast::Signature::Procedure(args) => {
                let args: Vec<_> = args
                    .iter_mut()
                    .map(|arg| self.resolve_type_mark(scope, arg))
                    .collect();
                (args, None)
            }
        };

        let mut params = Vec::with_capacity(args.len());
        for arg in args {
            params.push(arg?.base());
        }

        if let Some(return_type) = return_type {
            Ok(SignatureKey::new(
                params,
                Some(return_type?.base_type().base()),
            ))
        } else {
            Ok(SignatureKey::new(params, None))
        }
    }

    fn analyze_interface_declaration(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        decl: &mut InterfaceDeclaration,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<EntRef<'a>> {
        let ent = match decl {
            InterfaceDeclaration::File(ref mut file_decl) => {
                let file_type = self.resolve_subtype_indication(
                    scope,
                    &mut file_decl.subtype_indication,
                    diagnostics,
                )?;
                self.arena.define(
                    &mut file_decl.ident,
                    parent,
                    AnyEntKind::InterfaceFile(file_type.type_mark().to_owned()),
                )
            }
            InterfaceDeclaration::Object(ref mut object_decl) => {
                let subtype = self.resolve_subtype_indication(
                    scope,
                    &mut object_decl.subtype_indication,
                    diagnostics,
                );

                if let Some(ref mut expression) = object_decl.expression {
                    if let Ok(ref subtype) = subtype {
                        self.expr_pos_with_ttyp(
                            scope,
                            subtype.type_mark(),
                            &expression.pos,
                            &mut expression.item,
                            diagnostics,
                        )?;
                    } else {
                        self.expr_unknown_ttyp(scope, expression, diagnostics)?
                    }
                }

                let subtype = subtype?;
                self.arena.define(
                    &mut object_decl.ident,
                    parent,
                    AnyEntKind::Object(Object {
                        class: object_decl.class,
                        iface: Some(ObjectInterface::new(
                            object_decl.list_type,
                            object_decl.mode,
                        )),
                        subtype,
                        has_default: object_decl.expression.is_some(),
                    }),
                )
            }
            InterfaceDeclaration::Type(ref mut ident) => {
                let typ = TypeEnt::from_any(self.arena.define(
                    ident,
                    parent,
                    AnyEntKind::Type(Type::Interface),
                ))
                .unwrap();

                let implicit = [
                    self.comparison(Operator::EQ, typ),
                    self.comparison(Operator::NE, typ),
                ];

                for ent in implicit {
                    unsafe {
                        self.arena.add_implicit(typ.id(), ent);
                    }

                    scope.add(ent, diagnostics);
                }

                typ.into()
            }
            InterfaceDeclaration::Subprogram(ref mut subpgm, ..) => {
                let (_, ent) = self.subprogram_specification(
                    scope,
                    parent,
                    subpgm,
                    Overloaded::InterfaceSubprogram,
                    diagnostics,
                )?;
                ent.into()
            }
            InterfaceDeclaration::Package(ref mut instance) => {
                let package_region =
                    self.analyze_package_instance_name(scope, &mut instance.package_name)?;

                self.arena.define(
                    &mut instance.ident,
                    parent,
                    AnyEntKind::Design(Design::PackageInstance(package_region.clone())),
                )
            }
        };
        Ok(ent)
    }

    pub fn analyze_interface_list(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for decl in declarations.iter_mut() {
            match self.analyze_interface_declaration(scope, parent, decl, diagnostics) {
                Ok(ent) => {
                    scope.add(ent, diagnostics);
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }
        Ok(())
    }

    pub fn analyze_parameter_list(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        declarations: &mut [InterfaceDeclaration],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<FormalRegion<'a>> {
        let mut params = FormalRegion::new(InterfaceType::Parameter);

        for decl in declarations.iter_mut() {
            match self.analyze_interface_declaration(scope, parent, decl, diagnostics) {
                Ok(ent) => {
                    scope.add(ent, diagnostics);
                    params.add(ent);
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }
        Ok(params)
    }

    fn analyze_array_index(
        &self,
        scope: &Scope<'a>,
        array_index: &mut ArrayIndex,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> EvalResult<BaseType<'a>> {
        match array_index {
            ArrayIndex::IndexSubtypeDefintion(ref mut type_mark) => {
                match self.resolve_type_mark(scope, type_mark) {
                    Ok(typ) => Ok(typ.base()),
                    Err(err) => {
                        err.add_to(diagnostics)?;
                        Err(EvalError::Unknown)
                    }
                }
            }
            ArrayIndex::Discrete(ref mut drange) => self.drange_type(scope, drange, diagnostics),
        }
    }

    fn analyze_subtype_constraint(
        &self,
        scope: &Scope<'a>,
        pos: &SrcPos, // The position of the root type mark
        base_type: BaseType<'a>,
        constraint: &mut SubtypeConstraint,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match constraint {
            SubtypeConstraint::Array(ref mut dranges, ref mut constraint) => {
                if let Type::Array { indexes, elem_type } = base_type.kind() {
                    for (idx, drange) in dranges.iter_mut().enumerate() {
                        if let Some(index_typ) = indexes.get(idx) {
                            if let Some(index_typ) = index_typ {
                                self.drange_with_ttyp(
                                    scope,
                                    (*index_typ).into(),
                                    drange,
                                    diagnostics,
                                )?;
                            } else {
                                self.drange_unknown_type(scope, drange, diagnostics)?;
                            }
                        } else {
                            diagnostics.error(
                                drange.pos(),
                                format!("Got extra index constraint for {}", base_type.describe()),
                            );
                        }
                    }

                    // empty dranges means (open)
                    if dranges.len() < indexes.len() && !dranges.is_empty() {
                        diagnostics.error(
                            pos,
                            format!(
                                "Too few index constraints for {}. Got {} but expected {}",
                                base_type.describe(),
                                dranges.len(),
                                indexes.len()
                            ),
                        );
                    }

                    if let Some(constraint) = constraint {
                        self.analyze_subtype_constraint(
                            scope,
                            &constraint.pos,
                            elem_type.base(),
                            &mut constraint.item,
                            diagnostics,
                        )?;
                    }
                } else {
                    diagnostics.error(
                        pos,
                        format!(
                            "Array constraint cannot be used for {}",
                            base_type.describe()
                        ),
                    );
                }
            }
            SubtypeConstraint::Range(ref mut range) => {
                if base_type.is_scalar() {
                    self.range_with_ttyp(scope, base_type.into(), range, diagnostics)?;
                } else {
                    diagnostics.error(
                        pos,
                        format!(
                            "Scalar constraint cannot be used for {}",
                            base_type.describe()
                        ),
                    );
                }
            }
            SubtypeConstraint::Record(ref mut constraints) => {
                if let Type::Record(region) = base_type.kind() {
                    for constraint in constraints.iter_mut() {
                        let ElementConstraint { ident, constraint } = constraint;
                        let des = Designator::Identifier(ident.item.clone());
                        if let Some(elem) = region.lookup(&des) {
                            self.analyze_subtype_constraint(
                                scope,
                                &constraint.pos,
                                elem.type_mark().base(),
                                &mut constraint.item,
                                diagnostics,
                            )?;
                        } else {
                            diagnostics.push(Diagnostic::no_declaration_within(
                                &base_type, &ident.pos, &des,
                            ))
                        }
                    }
                } else {
                    diagnostics.error(
                        pos,
                        format!(
                            "Record constraint cannot be used for {}",
                            base_type.describe()
                        ),
                    );
                }
            }
        }
        Ok(())
    }

    pub fn resolve_subtype_indication(
        &self,
        scope: &Scope<'a>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<Subtype<'a>> {
        // @TODO more
        let SubtypeIndication {
            type_mark,
            constraint,
            ..
        } = subtype_indication;

        let base_type = self.resolve_type_mark(scope, type_mark)?;

        if let Some(constraint) = constraint {
            self.analyze_subtype_constraint(
                scope,
                &type_mark.pos,
                base_type.base(),
                &mut constraint.item,
                diagnostics,
            )?;
        }

        Ok(Subtype::new(base_type))
    }

    pub fn analyze_subtype_indication(
        &self,
        scope: &Scope<'a>,
        subtype_indication: &mut SubtypeIndication,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        if let Err(err) = self.resolve_subtype_indication(scope, subtype_indication, diagnostics) {
            err.add_to(diagnostics)?;
        }
        Ok(())
    }

    fn subprogram_header(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        header: &mut SubprogramHeader,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult<Region<'a>> {
        let mut region = Region::default();
        for decl in header.generic_list.iter_mut() {
            match self.analyze_interface_declaration(scope, parent, decl, diagnostics) {
                Ok(ent) => {
                    region.add(ent, diagnostics);
                    scope.add(ent, diagnostics);
                }
                Err(err) => {
                    err.add_to(diagnostics)?;
                }
            }
        }
        self.analyze_map_aspect(scope, &mut header.map_aspect, diagnostics)?;
        Ok(region)
    }

    fn subprogram_specification(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        subprogram: &mut SubprogramSpecification,
        to_kind: impl Fn(Signature<'a>) -> Overloaded<'a>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> AnalysisResult<(Scope<'a>, OverloadedEnt<'a>)> {
        let subpgm_region = scope.nested();
        let ent = self.arena.explicit(
            subprogram
                .subpgm_designator()
                .item
                .clone()
                .into_designator(),
            parent,
            AnyEntKind::Overloaded(to_kind(Signature::new(FormalRegion::new_params(), None))),
            Some(&subprogram.subpgm_designator().pos),
        );

        let (signature, generic_map) = match subprogram {
            SubprogramSpecification::Function(fun) => {
                let generic_map = if let Some(header) = &mut fun.header {
                    Some(self.subprogram_header(&subpgm_region, ent, header, diagnostics)?)
                } else {
                    None
                };
                let params = self.analyze_parameter_list(
                    &subpgm_region,
                    ent,
                    &mut fun.parameter_list,
                    diagnostics,
                );
                let return_type = self.resolve_type_mark(&subpgm_region, &mut fun.return_type);
                (Signature::new(params?, Some(return_type?)), generic_map)
            }
            SubprogramSpecification::Procedure(procedure) => {
                let generic_map = if let Some(header) = &mut procedure.header {
                    Some(self.subprogram_header(&subpgm_region, ent, header, diagnostics)?)
                } else {
                    None
                };
                let params = self.analyze_parameter_list(
                    &subpgm_region,
                    ent,
                    &mut procedure.parameter_list,
                    diagnostics,
                );
                (Signature::new(params?, None), generic_map)
            }
        };

        let mut kind = to_kind(signature);
        if let Some(map) = generic_map {
            match kind {
                Overloaded::SubprogramDecl(signature) => {
                    kind = Overloaded::UninstSubprogramDecl(signature, map)
                }
                Overloaded::Subprogram(signature) => {
                    kind = Overloaded::UninstSubprogram(signature, map)
                }
                _ => unreachable!(),
            }
        }

        match kind {
            Overloaded::Subprogram(_) => {
                let declared_by =
                    self.find_subpgm_specification(scope, subprogram, kind.signature());

                if let Some(declared_by) = declared_by {
                    unsafe {
                        ent.set_declared_by(declared_by.into());
                    }
                }
            }
            Overloaded::UninstSubprogram(_, _) => {
                let declared_by =
                    self.find_uninst_subpgm_specification(scope, subprogram, kind.signature());

                if let Some(declared_by) = declared_by {
                    unsafe {
                        ent.set_declared_by(declared_by.into());
                    }
                }
            }
            _ => {}
        }

        unsafe {
            ent.set_kind(AnyEntKind::Overloaded(kind));
        }
        subprogram.set_decl_id(ent.id());
        Ok((subpgm_region, OverloadedEnt::from_any(ent).unwrap()))
    }
}

fn find_full_type_definition<'a>(
    name: &Symbol,
    decls: &'a [Declaration],
) -> Option<&'a TypeDeclaration> {
    for decl in decls.iter() {
        if let Declaration::Type(type_decl) = decl {
            match type_decl.def {
                TypeDefinition::Incomplete(..) => {
                    // ignored
                }
                _ => {
                    if type_decl.ident.name() == name {
                        return Some(type_decl);
                    }
                }
            }
        }
    }
    None
}

impl Diagnostic {
    fn no_overloaded_with_signature(
        pos: &SrcPos,
        des: &Designator,
        overloaded: &OverloadedName,
    ) -> Diagnostic {
        let mut diagnostic = Diagnostic::error(
            pos,
            format!(
                "Could not find declaration of {} with given signature",
                des.describe()
            ),
        );
        diagnostic.add_subprogram_candidates("Found", overloaded.entities());
        diagnostic
    }

    fn should_not_have_signature(prefix: &str, pos: impl AsRef<SrcPos>) -> Diagnostic {
        Diagnostic::error(
            pos,
            format!("{prefix} should only have a signature for subprograms and enum literals"),
        )
    }

    fn signature_required(pos: impl AsRef<SrcPos>) -> Diagnostic {
        Diagnostic::error(
            pos,
            "Signature required for alias of subprogram and enum literals",
        )
    }
}

fn get_entity_class(ent: EntRef) -> Option<EntityClass> {
    match ent.actual_kind() {
        // Alias is never the direct target of attribute
        AnyEntKind::ExternalAlias { .. } => None,
        // Alias is never the direct target of attribute
        AnyEntKind::ObjectAlias { .. } => None,
        AnyEntKind::File(_) => Some(EntityClass::File),
        AnyEntKind::InterfaceFile(_) => Some(EntityClass::File),
        AnyEntKind::Component(_) => Some(EntityClass::Component),
        AnyEntKind::Attribute(_) => None,
        AnyEntKind::Overloaded(ent) => match ent {
            Overloaded::SubprogramDecl(s)
            | Overloaded::Subprogram(s)
            | Overloaded::UninstSubprogramDecl(s, _)
            | Overloaded::UninstSubprogram(s, _)
            | Overloaded::InterfaceSubprogram(s) => {
                if s.return_type.is_some() {
                    Some(EntityClass::Function)
                } else {
                    Some(EntityClass::Procedure)
                }
            }
            Overloaded::EnumLiteral(_) => Some(EntityClass::Literal),
            // Alias is never the direct target of attribute
            Overloaded::Alias(_) => None,
        },
        AnyEntKind::Type(Type::Subtype(_)) => Some(EntityClass::Subtype),
        AnyEntKind::Type(_) => Some(EntityClass::Type),
        AnyEntKind::ElementDeclaration(_) => None,
        AnyEntKind::Concurrent(_) => Some(EntityClass::Label),
        AnyEntKind::Sequential(_) => Some(EntityClass::Label),
        AnyEntKind::Object(obj) => match obj.class {
            ObjectClass::Signal => Some(EntityClass::Signal),
            ObjectClass::Constant => Some(EntityClass::Constant),
            ObjectClass::Variable => Some(EntityClass::Variable),
            ObjectClass::SharedVariable => Some(EntityClass::Variable),
        },
        AnyEntKind::LoopParameter(_) => None, // @TODO is it allowed?
        AnyEntKind::PhysicalLiteral(_) => None, // @TODO maybe Units?
        AnyEntKind::DeferredConstant(_) => Some(EntityClass::Constant),
        AnyEntKind::Library => None,
        AnyEntKind::Design(des) => match des {
            Design::Entity(_, _) => Some(EntityClass::Entity),
            Design::Architecture(_) => Some(EntityClass::Architecture),
            Design::Configuration => Some(EntityClass::Configuration),
            Design::Package(_, _) => Some(EntityClass::Package),
            // Should never be target of attribute
            Design::PackageBody => None,
            Design::UninstPackage(_, _) => None,
            Design::PackageInstance(_) => None,
            Design::Context(_) => None,
        },
    }
}
