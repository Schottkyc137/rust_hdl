// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

//! Compares the unmodified LRM grammar (`vhdl-08.ungram`) with the grammar that
//! `vhdl_syntax` actually models (`vhdl-08-modified.ungram`).
//!
//! Two things the modified grammar carries are not deviations from the standard, and
//! the comparison erases both so that what remains is a real difference in the
//! accepted language:
//!
//! * **Labels.** `generic_part:(GenericClause ...)?` and `(GenericClause ...)?` are
//!   the same rule.
//! * **Unmarked groups** (in the default [`Nesting::Flattened`] mode). A group that
//!   carries no `?` or `*` denotes exactly the concatenation of its elements, so
//!   `A (B C) D` and `A B C D` accept the same token sequences. The modified grammar
//!   has such groups only to give the codegen a node to hang error recovery,
//!   preambles/epilogues and `Parenthesized*` wrappers off.
//!
//! The erasure stops there, and deliberately. A group that *is* marked is never
//! flattened -- `(A B)?` is not `A? B?` -- and neither is an alternation, whose
//! nesting changes the language outright. Aliases (`Condition = Expression`),
//! alternative ordering and optional markers are likewise left alone: each of those
//! would hide a real divergence. Pass [`Nesting::Exact`] to compare the group nesting
//! as well.
//!
//! What is left is a list of real divergences, and every one of them is meant to be a
//! deliberate, explainable one. The [`explained`] module reads those explanations back
//! out of the developer book, so a difference somebody has written up stops taking up
//! room in the listing and is counted on its own -- and an explanation the grammar has
//! moved out from under is reported instead of quietly still counting.
//!
//! A production only one of the grammars has is a divergence like any other, and is
//! compared -- and explained -- like any other: against an [`absent`] body, so all of it
//! shows up as added or removed. Only its listing is shorter, by name rather than in
//! full, there being 60-odd of them.

use crate::diff::explained::{Claim, Explanation};
use anyhow::{Context, Result};
use similar::{ChangeTag, TextDiff};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::path::Path;
use std::str::FromStr;

mod explained;

/// Whether the comparison takes the nesting of unmarked groups into account.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Nesting {
    /// Compare the rules as written, so a group the modified grammar adds for scoping
    /// is itself a difference.
    Exact,
    /// Splice unmarked groups into their parent sequence before comparing.
    Flattened,
}

/// A grammar rule with labels erased.
#[derive(PartialEq, Eq, Clone)]
enum Rule {
    Node(String),
    Token(String),
    Seq(Vec<Rule>),
    Alt(Vec<Rule>),
    Opt(Box<Rule>),
    Rep(Box<Rule>),
}

impl Rule {
    fn from_ungrammar(rule: &ungrammar::Rule, grammar: &ungrammar::Grammar) -> Rule {
        let convert_all = |rules: &[ungrammar::Rule]| {
            rules
                .iter()
                .map(|r| Rule::from_ungrammar(r, grammar))
                .collect()
        };
        match rule {
            // The whole point of the diff: a label is not part of the grammar.
            ungrammar::Rule::Labeled { rule, .. } => Rule::from_ungrammar(rule, grammar),
            ungrammar::Rule::Node(node) => Rule::Node(grammar[*node].name.clone()),
            ungrammar::Rule::Token(token) => Rule::Token(grammar[*token].name.clone()),
            ungrammar::Rule::Seq(rules) => Rule::Seq(convert_all(rules)),
            ungrammar::Rule::Alt(rules) => Rule::Alt(convert_all(rules)),
            ungrammar::Rule::Opt(rule) => Rule::Opt(Box::new(Rule::from_ungrammar(rule, grammar))),
            ungrammar::Rule::Rep(rule) => Rule::Rep(Box::new(Rule::from_ungrammar(rule, grammar))),
        }
    }

    /// Splices every unmarked group into its parent sequence.
    ///
    /// Only a sequence nested directly in another sequence is spliced, because only
    /// there is the group unmarked and concatenation associative. The body of a `?`,
    /// a `*` or an alternative keeps its own parentheses -- dropping those would
    /// change the language -- but is descended into, so an unmarked group nested
    /// inside one is still flattened.
    fn flattened(&self) -> Rule {
        match self {
            Rule::Node(_) | Rule::Token(_) => self.clone(),
            Rule::Seq(rules) => {
                let mut flat = Vec::with_capacity(rules.len());
                for rule in rules {
                    match rule.flattened() {
                        Rule::Seq(inner) => flat.extend(inner),
                        other => flat.push(other),
                    }
                }
                Rule::Seq(flat)
            }
            Rule::Alt(rules) => Rule::Alt(rules.iter().map(Rule::flattened).collect()),
            Rule::Opt(rule) => Rule::Opt(Box::new(rule.flattened())),
            Rule::Rep(rule) => Rule::Rep(Box::new(rule.flattened())),
        }
    }

    /// The rule as the given mode compares it.
    fn as_compared(&self, nesting: Nesting) -> Rule {
        match nesting {
            Nesting::Exact => self.clone(),
            Nesting::Flattened => self.flattened(),
        }
    }

    /// Renders the rule on a single line, parenthesizing where the nesting would
    /// otherwise be lost.
    fn write_inline(&self, out: &mut String) {
        match self {
            Rule::Node(name) => out.push_str(name),
            Rule::Token(name) => {
                let _ = write!(out, "'{name}'");
            }
            Rule::Seq(rules) => {
                for (i, rule) in rules.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    rule.write_seq_element(out);
                }
            }
            Rule::Alt(rules) => {
                for (i, rule) in rules.iter().enumerate() {
                    if i > 0 {
                        out.push_str(" | ");
                    }
                    rule.write_inline(out);
                }
            }
            Rule::Opt(rule) => {
                rule.write_atom(out);
                out.push('?');
            }
            Rule::Rep(rule) => {
                rule.write_atom(out);
                out.push('*');
            }
        }
    }

    /// An element of a sequence: a nested sequence or alternation keeps its parentheses.
    fn write_seq_element(&self, out: &mut String) {
        match self {
            Rule::Seq(_) | Rule::Alt(_) => self.write_parenthesized(out),
            _ => self.write_inline(out),
        }
    }

    /// The operand of a `?` or `*`, which must be parenthesized unless it is a single item.
    fn write_atom(&self, out: &mut String) {
        match self {
            Rule::Node(_) | Rule::Token(_) => self.write_inline(out),
            _ => self.write_parenthesized(out),
        }
    }

    fn write_parenthesized(&self, out: &mut String) {
        out.push('(');
        self.write_inline(out);
        out.push(')');
    }

    /// Renders the rule as the body of a production, one element per line so that a
    /// line diff points at the element that changed.
    fn to_body_lines(&self) -> String {
        let mut out = String::new();
        match self {
            Rule::Seq(rules) => {
                for rule in rules {
                    out.push_str("  ");
                    rule.write_seq_element(&mut out);
                    out.push('\n');
                }
            }
            Rule::Alt(rules) => {
                for (i, rule) in rules.iter().enumerate() {
                    out.push_str(if i == 0 { "  " } else { "| " });
                    rule.write_inline(&mut out);
                    out.push('\n');
                }
            }
            _ => {
                out.push_str("  ");
                self.write_inline(&mut out);
                out.push('\n');
            }
        }
        out
    }
}

/// All productions of one grammar file, keyed by name.
fn load(file: &Path) -> Result<BTreeMap<String, Rule>> {
    let text =
        std::fs::read_to_string(file).with_context(|| format!("cannot read {}", file.display()))?;
    let grammar = ungrammar::Grammar::from_str(&text)
        .map_err(|err| anyhow::anyhow!("{}:{err}", file.display()))?;

    Ok(grammar
        .iter()
        .map(|node| {
            let data = &grammar[node];
            (
                data.name.clone(),
                Rule::from_ungrammar(&data.rule, &grammar),
            )
        })
        .collect())
}

/// The difference between the two rules, one element per line, exactly as it is
/// printed and as the book has to quote it for the difference to count as explained.
fn render_diff(lrm_rule: &Rule, modified_rule: &Rule) -> Vec<String> {
    let lrm_body = lrm_rule.to_body_lines();
    let modified_body = modified_rule.to_body_lines();
    let lines = TextDiff::from_lines(&lrm_body, &modified_body)
        .iter_all_changes()
        .map(|change| {
            let sign = match change.tag() {
                ChangeTag::Delete => '-',
                ChangeTag::Insert => '+',
                ChangeTag::Equal => ' ',
            };
            format!("{sign}{}", change.value())
        })
        .collect();
    explained::normalize(lines)
}

/// Prints the diff of the two grammars.
///
/// A difference the developer book in `book_dir` quotes verbatim is counted and listed
/// as *explained* rather than printed in full: it has been looked at, written down and
/// justified, so what stays in the listing is the part still awaiting that treatment.
pub fn diff_grammar(
    lrm_file: &Path,
    modified_file: &Path,
    book_dir: &Path,
    filter: Option<&str>,
    nesting: Nesting,
) -> Result<()> {
    let lrm = load(lrm_file)?;
    let modified = load(modified_file)?;
    // Under a filter the rest of the book is out of scope, and would otherwise all be
    // reported as unmatched.
    let explanations: Vec<Explanation> = explained::load(book_dir)?
        .into_iter()
        .filter(|explanation| filter.is_none_or(|filter| filter == explanation.production))
        .collect();
    let mut is_matched = vec![false; explanations.len()];

    let mut differing = Vec::new();
    let mut lrm_only = Vec::new();
    let mut modified_only = Vec::new();
    let mut explained = Vec::new();
    let mut identical = 0usize;
    // Identical only because the grouping was flattened away -- reported separately, so
    // that a relaxation of the comparison never passes for an exact match.
    let mut identical_when_flattened = 0usize;

    // Both grammars' productions, so that one only one of them has is compared -- and
    // can be explained -- like any other rather than only being counted at the end.
    let names: BTreeSet<&String> = lrm.keys().chain(modified.keys()).collect();
    for name in names {
        if filter.is_some_and(|filter| filter != name) {
            continue;
        }
        let (claim, lrm_rule, modified_rule) = match (lrm.get(name), modified.get(name)) {
            (Some(lrm_rule), Some(modified_rule)) => (
                Claim::Differing,
                lrm_rule.as_compared(nesting),
                modified_rule.as_compared(nesting),
            ),
            (Some(lrm_rule), None) => (Claim::OnlyInLrm, lrm_rule.as_compared(nesting), absent()),
            (None, Some(modified_rule)) => (
                Claim::OnlyInModified,
                absent(),
                modified_rule.as_compared(nesting),
            ),
            (None, None) => unreachable!("a name comes from one of the two grammars"),
        };

        if lrm_rule == modified_rule {
            identical += 1;
            if lrm.get(name) != modified.get(name) {
                identical_when_flattened += 1;
            }
            continue;
        }

        let diff = render_diff(&lrm_rule, &modified_rule);
        let explanation = explanations
            .iter()
            .position(|explanation| explains(explanation, name, claim, &diff));
        match explanation {
            Some(index) => {
                is_matched[index] = true;
                explained.push((name, &explanations[index]));
            }
            None => match claim {
                Claim::Differing => differing.push((name, diff)),
                Claim::OnlyInLrm => lrm_only.push((name, diff)),
                Claim::OnlyInModified => modified_only.push((name, diff)),
            },
        }
    }

    print_diffs(&differing);
    print_explained(&explained);
    print_unmatched(&explanations, &is_matched);

    if let Some(filter) = filter {
        // Nothing else is in scope, so the one production is worth spelling out in full
        // -- a fence quoting the body copies it from here.
        print_diffs(&lrm_only);
        print_diffs(&modified_only);
        match (lrm.contains_key(filter), modified.contains_key(filter)) {
            (false, false) => println!("no production named `{filter}` in either grammar"),
            (true, false) => println!("`{filter}` exists only in {}", file_name(lrm_file)),
            (false, true) => println!("`{filter}` exists only in {}", file_name(modified_file)),
            (true, true) if identical_when_flattened > 0 => {
                println!("`{filter}` is identical once unmarked groups are flattened")
            }
            (true, true) if identical > 0 => println!("`{filter}` is identical"),
            (true, true) => {}
        }
        return Ok(());
    }

    print_only_in(lrm_file, &lrm_only);
    print_only_in(modified_file, &modified_only);

    let flattened_note = if identical_when_flattened > 0 {
        format!(" ({identical_when_flattened} only after flattening unmarked groups)")
    } else {
        String::new()
    };
    let explained_note = if explained.is_empty() {
        String::new()
    } else {
        format!(", {} explained", explained.len())
    };
    println!(
        "{} differing, {identical} identical{flattened_note}, {} only in {}, {} only in {}{explained_note}",
        differing.len(),
        lrm_only.len(),
        file_name(lrm_file),
        modified_only.len(),
        file_name(modified_file),
    );

    Ok(())
}

/// Whether the explanation accounts for this difference.
///
/// It has to be about the same production and to say the same thing about it -- a
/// production the modified grammar dropped is not explained by prose about one it
/// added. A signed claim stops there, its whole content being that the production is
/// one-sided; a quoted diff has to match element for element, which is what makes it
/// go stale when the grammar moves.
fn explains(explanation: &Explanation, name: &str, claim: Claim, diff: &[String]) -> bool {
    explanation.production == name
        && explanation.claim == claim
        && (claim != Claim::Differing || explanation.diff == diff)
}

/// The rule a production only one grammar has is compared against: an empty body, so
/// every element of the one that is there shows up as a whole-production difference.
fn absent() -> Rule {
    Rule::Seq(Vec::new())
}

/// The differences still awaiting an explanation, spelled out one element per line.
fn print_diffs(differences: &[(&String, Vec<String>)]) {
    for (name, diff) in differences {
        println!("~~ {name} ~~");
        for line in diff {
            println!("{line}");
        }
        println!();
    }
}

/// The differences the book accounts for, named rather than spelled out.
fn print_explained(explained: &[(&String, &Explanation)]) {
    if explained.is_empty() {
        return;
    }
    println!("explained ({}):", explained.len());
    for (name, explanation) in explained {
        println!("  {name} ({})", explanation.location());
    }
    println!();
}

/// The explanations that account for no difference.
///
/// An explanation only counts while it describes the difference as it stands today, so
/// one that matches nothing is either prose about a grammar that no longer exists or a
/// second copy of an explanation that already did the job -- told apart by whether the
/// production is explained anywhere else.
fn print_unmatched(explanations: &[Explanation], is_matched: &[bool]) {
    let mut stale = Vec::new();
    let mut duplicated = Vec::new();
    for (explanation, matched) in explanations.iter().zip(is_matched) {
        if *matched {
            continue;
        }
        // Same production *and* same claim: a `+ X` that fell through where an
        // unsigned `X` matched is not a second copy, it is a claim the grammar denies.
        let accounted_for = explanations
            .iter()
            .zip(is_matched)
            .find(|(other, matched)| {
                **matched
                    && other.production == explanation.production
                    && other.claim == explanation.claim
            });
        match accounted_for {
            Some((first, _)) => duplicated.push((explanation, first)),
            None => stale.push(explanation),
        }
    }

    if !stale.is_empty() {
        println!(
            "explaining a difference that is no longer there ({}):",
            stale.len()
        );
        for explanation in &stale {
            println!("  {} `{}`", explanation.location(), explanation.production);
        }
        println!("update it to what this run prints, or drop it.");
        println!();
    }

    if !duplicated.is_empty() {
        println!("explained twice ({}):", duplicated.len());
        for (explanation, first) in &duplicated {
            println!(
                "  {} `{}`, already explained at {}",
                explanation.location(),
                explanation.production,
                first.location()
            );
        }
        println!();
    }
}

/// The productions only one grammar has, by name: all of such a production is the
/// difference, and there are enough of them that printing each in full would bury the
/// rest of the listing. `--production` prints the one it is asked about.
fn print_only_in(file: &Path, differences: &[(&String, Vec<String>)]) {
    if differences.is_empty() {
        return;
    }
    println!("only in {} ({}):", file_name(file), differences.len());
    for (name, _) in differences {
        println!("  {name}");
    }
    println!();
}

fn file_name(file: &Path) -> String {
    file.file_name()
        .unwrap_or_default()
        .to_string_lossy()
        .into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The body of `Production` in `grammar`, rendered back on one line.
    fn rule_of(grammar: &str, production: &str) -> Rule {
        let grammar = ungrammar::Grammar::from_str(grammar).expect("test grammar does not parse");
        let node = grammar
            .iter()
            .find(|node| grammar[*node].name == production)
            .unwrap_or_else(|| panic!("no production named {production}"));
        Rule::from_ungrammar(&grammar[node].rule, &grammar)
    }

    /// The one explanation in a markdown snippet.
    fn parsed_claim(text: &str) -> Explanation {
        explained::parse(text, "test.md")
            .expect("fence does not parse")
            .pop()
            .expect("snippet holds no fence")
    }

    fn flattened(grammar: &str, production: &str) -> String {
        let mut out = String::new();
        rule_of(grammar, production)
            .flattened()
            .write_inline(&mut out);
        out
    }

    #[test]
    fn an_unmarked_group_is_spliced_into_its_parent() {
        let grammar = "
            X = 'a' preamble:('b' 'c') 'd'
        ";
        assert_eq!(flattened(grammar, "X"), "'a' 'b' 'c' 'd'");
    }

    #[test]
    fn an_optional_group_keeps_its_parentheses() {
        let grammar = "
            X = 'a' epilogue:('b' 'c')? 'd'
        ";
        assert_eq!(flattened(grammar, "X"), "'a' ('b' 'c')? 'd'");
    }

    #[test]
    fn a_repeated_group_keeps_its_parentheses() {
        let grammar = "
            X = 'a' elsif:('b' 'c')*
        ";
        assert_eq!(flattened(grammar, "X"), "'a' ('b' 'c')*");
    }

    #[test]
    fn an_alternation_is_not_spliced() {
        let grammar = "
            X = lhs:('a' 'b') | rhs:('c' 'd')
        ";
        assert_eq!(flattened(grammar, "X"), "'a' 'b' | 'c' 'd'");
    }

    #[test]
    fn an_unmarked_group_inside_a_marked_one_is_still_spliced() {
        let grammar = "
            X = elsif:('a' inner:('b' 'c') 'd')*
        ";
        assert_eq!(flattened(grammar, "X"), "('a' 'b' 'c' 'd')*");
    }

    #[test]
    fn a_parenthesized_list_group_flattens_to_the_lrm_spelling() {
        let grammar = "
            Aggregate = '(' items:(Element (',' Element)*) ')'
            Element = '#identifier'
        ";
        assert_eq!(
            flattened(grammar, "Aggregate"),
            "'(' Element (',' Element)* ')'"
        );
    }

    /// The two halves of the explanation check -- what the tool renders and what the
    /// book quotes -- have to agree line for line, so this pins the shape they meet on.
    #[test]
    fn a_fence_quoting_the_rendered_diff_compares_equal() {
        let diff = render_diff(&rule_of("X = 'a' 'b'", "X"), &rule_of("X = 'a' 'c'", "X"));
        assert_eq!(diff, ["   'a'", "-  'b'", "+  'c'"]);

        let fence = "```diff\nX =\n   'a'\n-  'b'\n+  'c'\n```\n";
        let explanations = explained::parse(fence, "test.md").expect("fence does not parse");
        assert_eq!(explanations[0].production, "X");
        assert_eq!(explanations[0].diff, diff);
    }

    /// The whole of a one-sided production is the difference, and it is rendered as
    /// such: a diff against a body with no elements at all.
    #[test]
    fn a_production_only_one_grammar_has_is_diffed_against_an_empty_body() {
        let grammar = "
            BinaryExpression = Lhs BinaryOperator Rhs
            Lhs = '#identifier'
            BinaryOperator = '+'
            Rhs = '#identifier'
        ";
        let rule = rule_of(grammar, "BinaryExpression");
        assert_eq!(
            render_diff(&absent(), &rule),
            ["+  Lhs", "+  BinaryOperator", "+  Rhs"]
        );
        assert_eq!(
            render_diff(&rule, &absent()),
            ["-  Lhs", "-  BinaryOperator", "-  Rhs"]
        );
    }

    /// A claim is about the production's presence, so it stays valid while the
    /// production it is about is reshaped -- there is no body for the grammar to move
    /// out from under.
    #[test]
    fn a_claim_explains_a_one_sided_production_whatever_its_body() {
        let explanation = &parsed_claim("```diff\n+ X\n```\n");
        let one = render_diff(&absent(), &rule_of("X = 'a' 'b'", "X"));
        let another = render_diff(&absent(), &rule_of("X = 'c'", "X"));
        assert!(explains(explanation, "X", Claim::OnlyInModified, &one));
        assert!(explains(explanation, "X", Claim::OnlyInModified, &another));
    }

    /// The sign says which grammar has the production, so the two directions -- and the
    /// unsigned form, which is about a production both grammars have -- never stand in
    /// for one another.
    #[test]
    fn an_explanation_only_accounts_for_the_claim_it_makes() {
        let diff = render_diff(&absent(), &rule_of("X = 'a'", "X"));
        let added = parsed_claim("```diff\n+ X\n```\n");
        let removed = parsed_claim("```diff\n- X\n```\n");
        let differing = parsed_claim("```diff\nX =\n+  'a'\n```\n");
        assert!(!explains(&removed, "X", Claim::OnlyInModified, &diff));
        assert!(!explains(&differing, "X", Claim::OnlyInModified, &diff));
        assert!(!explains(&added, "X", Claim::OnlyInLrm, &diff));
        assert!(!explains(&added, "Y", Claim::OnlyInModified, &diff));
    }

    #[test]
    fn a_fence_quoting_a_stale_diff_does_not_compare_equal() {
        let diff = render_diff(&rule_of("X = 'a' 'b'", "X"), &rule_of("X = 'a' 'c'", "X"));
        let fence = "```diff\nX =\n   'a'\n-  'b'\n+  'd'\n```\n";
        let explanations = explained::parse(fence, "test.md").expect("fence does not parse");
        assert_ne!(explanations[0].diff, diff);
    }

    #[test]
    fn a_flat_rule_is_unchanged() {
        let grammar = "
            X = 'a' Y? 'b'* ('c' | 'd')
            Y = '#identifier'
        ";
        let mut written = String::new();
        rule_of(grammar, "X").write_inline(&mut written);
        assert_eq!(flattened(grammar, "X"), written);
        assert!(rule_of(grammar, "X").flattened().flattened() == rule_of(grammar, "X").flattened());
    }
}
