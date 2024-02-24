// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

use super::*;
use crate::ast::*;
use crate::data::*;
use crate::named_entity::*;
use analyze::*;
use target::AssignmentType;

impl<'a> AnalyzeContext<'a> {
    pub fn define_labels_for_sequential_part(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        statements: &mut [LabeledSequentialStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for statement in statements.iter_mut() {
            let parent = if let Some(ref mut label) = statement.label.tree {
                let ent = self.arena.explicit(
                    label.name(),
                    parent,
                    AnyEntKind::Sequential(statement.statement.item.label_typ()),
                    Some(label.pos()),
                    None,
                );
                statement.label.decl.set(ent.id());
                scope.add(ent, diagnostics);
                ent
            } else if statement.statement.item.can_have_label() {
                // Generate an anonymous label if it is not explicitly defined
                let ent = self.arena.alloc(
                    Designator::Anonymous(scope.next_anonymous()),
                    Some(parent),
                    Related::None,
                    AnyEntKind::Sequential(statement.statement.item.label_typ()),
                    None,
                    None,
                );
                statement.label.decl.set(ent.id());
                ent
            } else {
                parent
            };

            match statement.statement.item {
                SequentialStatement::If(ref mut ifstmt) => {
                    let Conditionals {
                        conditionals,
                        else_item,
                    } = &mut ifstmt.conds;

                    for conditional in conditionals {
                        self.define_labels_for_sequential_part(
                            scope,
                            parent,
                            &mut conditional.item,
                            diagnostics,
                        )?;
                    }
                    if let Some(else_item) = else_item {
                        self.define_labels_for_sequential_part(
                            scope,
                            parent,
                            else_item,
                            diagnostics,
                        )?;
                    }
                }

                SequentialStatement::Case(ref mut case_stmt) => {
                    for alternative in case_stmt.alternatives.iter_mut() {
                        self.define_labels_for_sequential_part(
                            scope,
                            parent,
                            &mut alternative.item,
                            diagnostics,
                        )?;
                    }
                }
                SequentialStatement::Loop(ref mut loop_stmt) => {
                    self.define_labels_for_sequential_part(
                        scope,
                        parent,
                        &mut loop_stmt.statements,
                        diagnostics,
                    )?;
                }
                _ => {
                    // Does not have sequential part
                }
            }
        }

        Ok(())
    }

    fn analyze_sequential_statement(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        statement: &mut LabeledSequentialStatement,
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        match statement.statement.item {
            SequentialStatement::Return(ref mut ret) => {
                let ReturnStatement { ref mut expression } = ret;

                match SequentialRoot::from(parent) {
                    SequentialRoot::Function(ttyp) => {
                        if let Some(ref mut expression) = expression {
                            self.expr_with_ttyp(scope, ttyp, expression, diagnostics)?;
                        } else {
                            diagnostics.error(
                                &statement.statement.pos,
                                "Functions cannot return without a value",
                            );
                        }
                    }
                    SequentialRoot::Procedure => {
                        if expression.is_some() {
                            diagnostics.error(
                                &statement.statement.pos,
                                "Procedures cannot return a value",
                            );
                        }
                    }
                    SequentialRoot::Process => {
                        diagnostics.error(&statement.statement.pos, "Cannot return from a process");
                    }
                }
            }
            SequentialStatement::Wait(ref mut wait_stmt) => {
                let WaitStatement {
                    sensitivity_clause,
                    condition_clause,
                    timeout_clause,
                } = wait_stmt;
                self.sensitivity_list_check(scope, sensitivity_clause, diagnostics)?;
                if let Some(expr) = condition_clause {
                    self.boolean_expr(scope, expr, diagnostics)?;
                }
                if let Some(expr) = timeout_clause {
                    self.expr_with_ttyp(scope, self.time(), expr, diagnostics)?;
                }
            }
            SequentialStatement::Assert(ref mut assert_stmt) => {
                let AssertStatement {
                    condition,
                    report,
                    severity,
                } = assert_stmt;
                self.boolean_expr(scope, condition, diagnostics)?;
                if let Some(expr) = report {
                    self.expr_with_ttyp(scope, self.string(), expr, diagnostics)?;
                }
                if let Some(expr) = severity {
                    self.expr_with_ttyp(scope, self.severity_level(), expr, diagnostics)?;
                }
            }
            SequentialStatement::Report(ref mut report_stmt) => {
                let ReportStatement { report, severity } = report_stmt;
                self.expr_with_ttyp(scope, self.string(), report, diagnostics)?;
                if let Some(expr) = severity {
                    self.expr_with_ttyp(scope, self.severity_level(), expr, diagnostics)?;
                }
            }
            SequentialStatement::Exit(ref mut exit_stmt) => {
                let ExitStatement {
                    condition,
                    loop_label,
                } = exit_stmt;

                if let Some(loop_label) = loop_label {
                    self.check_loop_label(scope, parent, loop_label, diagnostics);
                } else if !find_outer_loop(parent, None) {
                    diagnostics.error(
                        &statement.statement.pos,
                        "Exit can only be used inside a loop",
                    )
                }

                if let Some(expr) = condition {
                    self.boolean_expr(scope, expr, diagnostics)?;
                }
            }
            SequentialStatement::Next(ref mut next_stmt) => {
                let NextStatement {
                    condition,
                    loop_label,
                } = next_stmt;

                if let Some(loop_label) = loop_label {
                    self.check_loop_label(scope, parent, loop_label, diagnostics);
                } else if !find_outer_loop(parent, None) {
                    diagnostics.error(
                        &statement.statement.pos,
                        "Next can only be used inside a loop",
                    )
                }

                if let Some(expr) = condition {
                    self.boolean_expr(scope, expr, diagnostics)?;
                }
            }
            SequentialStatement::If(ref mut ifstmt) => {
                let Conditionals {
                    conditionals,
                    else_item,
                } = &mut ifstmt.conds;

                // @TODO write generic function for this
                for conditional in conditionals {
                    let Conditional { condition, item } = conditional;
                    self.boolean_expr(scope, condition, diagnostics)?;
                    self.analyze_sequential_part(scope, parent, item, diagnostics)?;
                }
                if let Some(else_item) = else_item {
                    self.analyze_sequential_part(scope, parent, else_item, diagnostics)?;
                }
            }
            SequentialStatement::Case(ref mut case_stmt) => {
                let CaseStatement {
                    is_matching: _,
                    expression,
                    alternatives,
                    end_label_pos: _,
                } = case_stmt;
                let ctyp = as_fatal(self.expr_unambiguous_type(scope, expression, diagnostics))?;
                for alternative in alternatives.iter_mut() {
                    let Alternative { choices, item } = alternative;
                    self.choice_with_ttyp(scope, ctyp, choices, diagnostics)?;
                    self.analyze_sequential_part(scope, parent, item, diagnostics)?;
                }
            }
            SequentialStatement::Loop(ref mut loop_stmt) => {
                let LoopStatement {
                    iteration_scheme,
                    statements,
                    end_label_pos: _,
                } = loop_stmt;
                match iteration_scheme {
                    Some(IterationScheme::For(ref mut index, ref mut drange)) => {
                        let typ = as_fatal(self.drange_type(scope, drange, diagnostics))?;
                        let region = scope.nested();
                        region.add(
                            self.arena
                                .define(index, parent, AnyEntKind::LoopParameter(typ), None),
                            diagnostics,
                        );
                        self.analyze_sequential_part(&region, parent, statements, diagnostics)?;
                    }
                    Some(IterationScheme::While(ref mut expr)) => {
                        self.boolean_expr(scope, expr, diagnostics)?;
                        self.analyze_sequential_part(scope, parent, statements, diagnostics)?;
                    }
                    None => {
                        self.analyze_sequential_part(scope, parent, statements, diagnostics)?;
                    }
                }
            }
            SequentialStatement::ProcedureCall(ref mut pcall) => {
                self.analyze_procedure_call(scope, pcall, diagnostics)?;
            }
            SequentialStatement::SignalAssignment(ref mut assign) => {
                // @TODO more
                let SignalAssignment { target, rhs, .. } = assign;
                self.analyze_waveform_assignment(
                    scope,
                    target,
                    AssignmentType::Signal,
                    rhs,
                    diagnostics,
                )?;
            }
            SequentialStatement::VariableAssignment(ref mut assign) => {
                let VariableAssignment { target, rhs } = assign;
                self.analyze_expr_assignment(
                    scope,
                    target,
                    AssignmentType::Variable,
                    rhs,
                    diagnostics,
                )?;
            }
            SequentialStatement::SignalForceAssignment(ref mut assign) => {
                let SignalForceAssignment {
                    target,
                    force_mode: _,
                    rhs,
                } = assign;
                self.analyze_expr_assignment(
                    scope,
                    target,
                    AssignmentType::Signal,
                    rhs,
                    diagnostics,
                )?;
            }
            SequentialStatement::SignalReleaseAssignment(ref mut assign) => {
                let SignalReleaseAssignment {
                    target,
                    force_mode: _,
                } = assign;
                as_fatal(self.resolve_target(scope, target, AssignmentType::Signal, diagnostics))?;
            }
            SequentialStatement::Null => {}
        }
        Ok(())
    }

    fn check_loop_label(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        label: &mut WithRef<Ident>,
        diagnostics: &mut dyn DiagnosticHandler,
    ) {
        match scope.lookup(
            &label.item.pos,
            &Designator::Identifier(label.item.item.clone()),
        ) {
            Ok(NamedEntities::Single(ent)) => {
                label.set_unique_reference(ent);
                if matches!(ent.kind(), AnyEntKind::Sequential(Some(Sequential::Loop))) {
                    if !find_outer_loop(parent, Some(label.item.name())) {
                        diagnostics.error(
                            &label.item.pos,
                            format!("Cannot be used outside of loop '{}'", ent.designator()),
                        );
                    }
                } else {
                    diagnostics.error(
                        &label.item.pos,
                        format!("Expected loop label, got {}", ent.describe()),
                    );
                }
            }
            Ok(NamedEntities::Overloaded(_)) => diagnostics.error(
                &label.item.pos,
                format!(
                    "Expected loop label, got overloaded name {}",
                    &label.item.item
                ),
            ),
            Err(diag) => {
                diagnostics.push(diag);
            }
        }
    }

    pub fn analyze_sequential_part(
        &self,
        scope: &Scope<'a>,
        parent: EntRef<'a>,
        statements: &mut [LabeledSequentialStatement],
        diagnostics: &mut dyn DiagnosticHandler,
    ) -> FatalResult {
        for statement in statements.iter_mut() {
            let parent = if let Some(id) = statement.label.decl.get() {
                self.arena.get(id)
            } else {
                parent
            };

            self.analyze_sequential_statement(scope, parent, statement, diagnostics)?;
        }

        Ok(())
    }
}

enum SequentialRoot<'a> {
    Process,
    Procedure,
    Function(TypeEnt<'a>),
}

fn find_outer_loop(ent: EntRef, label: Option<&Symbol>) -> bool {
    match ent.kind() {
        AnyEntKind::Sequential(Some(Sequential::Loop)) => {
            if let Some(label) = label {
                if matches!(ent.designator(), Designator::Identifier(ident) if ident == label) {
                    return true;
                }
            } else {
                return true;
            }
        }
        AnyEntKind::Sequential(_) => {}
        _ => {
            return false;
        }
    }

    if let Some(parent) = ent.parent {
        find_outer_loop(parent, label)
    } else {
        false
    }
}

impl<'a> From<EntRef<'a>> for SequentialRoot<'a> {
    fn from(value: EntRef<'a>) -> Self {
        match value.kind() {
            AnyEntKind::Overloaded(overloaded) => {
                if let Some(return_type) = overloaded.signature().return_type() {
                    SequentialRoot::Function(return_type)
                } else {
                    SequentialRoot::Procedure
                }
            }
            AnyEntKind::Sequential(_) => {
                if let Some(parent) = value.parent {
                    SequentialRoot::from(parent)
                } else {
                    // A sequential statement must always have a parent this should never happen
                    SequentialRoot::Process
                }
            }
            AnyEntKind::Concurrent(Some(Concurrent::Process)) => SequentialRoot::Process,
            _ => SequentialRoot::Process,
        }
    }
}
