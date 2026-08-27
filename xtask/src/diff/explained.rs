// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

//! Reads the developer book's write-ups of the differences between the LRM grammar
//! and the grammar `vhdl_syntax` models.
//!
//! Every markdown file below `book/src/lrm-differences/` except the chapter's
//! `README.md` may quote a difference in a fenced `diff` block, headed by the
//! production it is about:
//!
//! ```text
//! ```diff
//! DesignFile =
//!    DesignUnit
//!    DesignUnit*
//! +  '#eof'
//! ```
//! ```
//!
//! The quoted diff has to be exactly what [`super::diff_grammar`] renders for that
//! production, so a difference counts as *explained* only for as long as the prose
//! still describes the grammar. Once the grammar moves on, the fence stops matching
//! and is reported as stale rather than quietly going on being counted.
//!
//! A production that exists on only one side has no such per-element diff -- all of it
//! is the difference -- so the sign moves onto the name and the body falls away. What
//! is left is short enough that one fence carries the whole family:
//!
//! ```text
//! ```diff
//! + BinaryExpression
//! + UnaryExpression
//! - Term
//! - Factor
//! ```
//! ```
//!
//! `+` claims the production exists only in the modified grammar and `-` only in the
//! LRM's, and each line is one such claim, checked like any other: a fence outlives the
//! grammar no longer here than it does there. See [`Claim`].
//!
//! The two shapes do not mix. A fence heads a diff or it lists claims, and which one it
//! is is decided by its first line -- otherwise `+ Lhs` could as well be the second line
//! of a quoted body as a claim of its own.

use anyhow::{bail, Context, Result};
use std::path::Path;

/// What a fence says the difference *is*, which is the first thing
/// [`super::diff_grammar`] checks it against.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Claim {
    /// `DesignFile =` -- the production is in both grammars and their bodies differ.
    Differing,
    /// `+ BinaryExpression` -- the production exists only in the modified grammar.
    OnlyInModified,
    /// `- Primary` -- the production exists only in the LRM grammar.
    OnlyInLrm,
}

/// One difference somebody wrote the reason for: a whole fence when it heads a diff,
/// one of its lines when it lists claims.
#[derive(Debug)]
pub struct Explanation {
    /// The production the fence names (`DesignFile =`, `+ BinaryExpression`).
    pub production: String,
    /// What the fence says about that production.
    pub claim: Claim,
    /// The diff below the header, [`normalize`]d so it can be compared line by line
    /// with the diff the tool renders. Empty for a [`Claim::OnlyInModified`] or
    /// [`Claim::OnlyInLrm`] claim, which is about the production's presence rather
    /// than its body.
    pub diff: Vec<String>,
    /// The file the fence came from, for reporting.
    file: String,
    /// 1-based line the explanation starts at, for reporting.
    line: usize,
}

impl Explanation {
    /// `design.md:3`, the way an editor jumps to it.
    pub fn location(&self) -> String {
        format!("{}:{}", self.file, self.line)
    }
}

/// Every explanation in `dir`, in file then document order.
pub fn load(dir: &Path) -> Result<Vec<Explanation>> {
    let entries =
        std::fs::read_dir(dir).with_context(|| format!("cannot read {}", dir.display()))?;

    let mut files = Vec::new();
    for entry in entries {
        let path = entry
            .with_context(|| format!("cannot read {}", dir.display()))?
            .path();
        // The chapter's own introduction explains the idea, not a single difference.
        let is_chapter_intro = path.file_name().is_some_and(|name| name == "README.md");
        if path.extension().is_some_and(|ext| ext == "md") && !is_chapter_intro {
            files.push(path);
        }
    }
    files.sort();

    let mut explanations = Vec::new();
    for path in &files {
        let text = std::fs::read_to_string(path)
            .with_context(|| format!("cannot read {}", path.display()))?;
        let name = path.file_name().unwrap_or_default().to_string_lossy();
        explanations.extend(parse(&text, &name)?);
    }
    Ok(explanations)
}

/// Trims the trailing whitespace of every line and drops the blank lines around the
/// block, so that a fence indented or padded by an editor still compares equal.
/// Leading whitespace is left alone -- it is what tells a diff's `-`, `+` and context
/// lines apart from one another.
pub fn normalize(lines: Vec<String>) -> Vec<String> {
    let mut lines: Vec<String> = lines
        .into_iter()
        .map(|line| line.trim_end().to_owned())
        .collect();
    while lines.last().is_some_and(String::is_empty) {
        lines.pop();
    }
    let leading = lines.iter().take_while(|line| line.is_empty()).count();
    lines.drain(..leading);
    lines
}

/// The fenced `diff` blocks of one markdown file.
///
/// Fences carrying any other info string are skipped whole, so a `diff` fence is never
/// found inside one.
pub(super) fn parse(text: &str, file: &str) -> Result<Vec<Explanation>> {
    let mut explanations = Vec::new();
    let mut lines = text.lines().enumerate();

    while let Some((index, line)) = lines.next() {
        let Some(info) = line.trim().strip_prefix("```") else {
            continue;
        };
        let mut body = Vec::new();
        let mut terminated = false;
        for (_, line) in lines.by_ref() {
            if line.trim() == "```" {
                terminated = true;
                break;
            }
            body.push(line.to_owned());
        }

        if info.trim() != "diff" {
            continue;
        }
        let line = index + 1;
        if !terminated {
            bail!("{file}:{line}: unterminated diff fence");
        }
        explanations.extend(explanations_of(body, file, line)?);
    }

    Ok(explanations)
}

/// Splits a fence into the explanations it carries.
///
/// Its first line decides which of the two shapes it has: a production name heads the
/// diff of a production both grammars have, a signed name makes the whole fence a list
/// of productions only one grammar has.
fn explanations_of(mut body: Vec<String>, file: &str, line: usize) -> Result<Vec<Explanation>> {
    let Some(first) = body.first() else {
        bail!("{file}:{line}: empty diff fence");
    };
    if signed(first).is_some() {
        return claims(&body, file, line);
    }

    let header = body.remove(0);
    let Some(production) = production_name(&header) else {
        bail!(
            "{file}:{line}: a diff fence either heads the diff of one production, \
             written like `DesignFile =`, or lists the productions only one grammar \
             has, one `+ Name` or `- Name` per line; found `{header}`"
        );
    };

    let diff = normalize(body);
    // Nothing else in the fence says which elements changed, so without a body there is
    // no difference left to explain.
    if diff.is_empty() {
        bail!(
            "{file}:{line}: `{production}` heads no diff; a fence for a production only \
             one grammar has is a signed list, `+ {production}` or `- {production}`"
        );
    }

    Ok(vec![Explanation {
        production: production.to_owned(),
        claim: Claim::Differing,
        diff,
        file: file.to_owned(),
        line,
    }])
}

/// One explanation per signed line, each reported at the line it is written on.
fn claims(body: &[String], file: &str, line: usize) -> Result<Vec<Explanation>> {
    let mut claims = Vec::new();
    for (offset, text) in body.iter().enumerate() {
        // Blank lines are the writer's spacing, not a claim.
        if text.trim().is_empty() {
            continue;
        }
        let line = line + 1 + offset;
        let named =
            signed(text).and_then(|(claim, rest)| Some((claim, production_name(rest)?.to_owned())));
        let Some((claim, production)) = named else {
            bail!(
                "{file}:{line}: a fence that starts with a `+` or a `-` lists the \
                 productions only one grammar has, one per line, written like \
                 `+ BinaryExpression` or `- Primary`; found `{text}`"
            );
        };
        claims.push(Explanation {
            production,
            claim,
            diff: Vec::new(),
            file: file.to_owned(),
            line,
        });
    }
    Ok(claims)
}

/// The claim a leading `+` or `-` makes, and the rest of the line it made it about.
fn signed(line: &str) -> Option<(Claim, &str)> {
    let line = line.trim_start();
    if let Some(rest) = line.strip_prefix('+') {
        Some((Claim::OnlyInModified, rest))
    } else if let Some(rest) = line.strip_prefix('-') {
        Some((Claim::OnlyInLrm, rest))
    } else {
        None
    }
}

/// The production `text` names, if a production name with an optional `=` is all it is.
fn production_name(text: &str) -> Option<&str> {
    let name = text.trim().trim_end_matches('=').trim();
    let is_name = !name.is_empty() && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');
    is_name.then_some(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parsed(text: &str) -> Vec<Explanation> {
        parse(text, "test.md").expect("fence does not parse")
    }

    #[test]
    fn a_fence_is_the_production_it_heads_and_the_diff_it_quotes() {
        let explanations = parsed(
            "# Design\n\
             \n\
             ```diff\n\
             DesignFile =\n\
             \x20  DesignUnit\n\
             +  '#eof'\n\
             ```\n\
             \n\
             prose\n",
        );
        assert_eq!(explanations.len(), 1);
        assert_eq!(explanations[0].production, "DesignFile");
        assert_eq!(explanations[0].diff, ["   DesignUnit", "+  '#eof'"]);
        assert_eq!(explanations[0].location(), "test.md:3");
    }

    #[test]
    fn a_header_may_trail_whitespace_after_its_equals_sign() {
        let explanations = parsed("```diff\nDesignFile = \n+  '#eof'\n```\n");
        assert_eq!(explanations[0].production, "DesignFile");
        assert_eq!(explanations[0].diff, ["+  '#eof'"]);
    }

    #[test]
    fn several_fences_in_one_file_are_all_read() {
        let explanations = parsed("```diff\nA =\n+  'a'\n```\n\n```diff\nB =\n+  'b'\n```\n");
        let names: Vec<_> = explanations.iter().map(|e| e.production.as_str()).collect();
        assert_eq!(names, ["A", "B"]);
    }

    #[test]
    fn a_fence_of_another_language_is_skipped_whole() {
        let explanations = parsed("```rust\n```diff\nA =\n```\n```\n");
        assert!(explanations.is_empty());
    }

    #[test]
    fn a_fence_is_the_claim_that_a_production_is_only_in_the_modified_grammar() {
        let explanations = parsed("```diff\n+ BinaryExpression\n```\n");
        assert_eq!(explanations[0].production, "BinaryExpression");
        assert_eq!(explanations[0].claim, Claim::OnlyInModified);
        assert!(explanations[0].diff.is_empty());
    }

    #[test]
    fn a_fence_is_the_claim_that_a_production_is_only_in_the_lrm_grammar() {
        let explanations = parsed("```diff\n- Primary =\n```\n");
        assert_eq!(explanations[0].production, "Primary");
        assert_eq!(explanations[0].claim, Claim::OnlyInLrm);
    }

    #[test]
    fn a_fence_of_signed_names_is_one_claim_per_line() {
        let explanations = parsed(
            "```diff\n\
             + BinaryExpression\n\
             + UnaryExpression\n\
             \n\
             - Term\n\
             - Factor\n\
             ```\n",
        );
        let claims: Vec<_> = explanations
            .iter()
            .map(|e| (e.production.as_str(), e.claim))
            .collect();
        assert_eq!(
            claims,
            [
                ("BinaryExpression", Claim::OnlyInModified),
                ("UnaryExpression", Claim::OnlyInModified),
                ("Term", Claim::OnlyInLrm),
                ("Factor", Claim::OnlyInLrm),
            ]
        );
        assert!(explanations.iter().all(|e| e.diff.is_empty()));
    }

    /// Each line is its own explanation, so a stale one is reported where it is written
    /// rather than at the fence that happens to carry it.
    #[test]
    fn a_claim_is_reported_at_its_own_line() {
        let explanations = parsed("prose\n\n```diff\n+ A\n+ B\n```\n");
        assert_eq!(explanations[0].location(), "test.md:4");
        assert_eq!(explanations[1].location(), "test.md:5");
    }

    /// The first line decides the shape, so what would otherwise be a body line is a
    /// claim -- and a body line that names no single production is an error.
    #[test]
    fn a_fence_of_signed_names_takes_no_body() {
        let err = parse("```diff\n+ X\n+  'a' 'b'\n```\n", "test.md").unwrap_err();
        assert!(err.to_string().contains("one per line"));
        assert!(err.to_string().contains("test.md:3"));
    }

    #[test]
    fn an_unsigned_header_is_the_diff_of_a_production_both_grammars_have() {
        let explanations = parsed("```diff\nDesignFile =\n+  '#eof'\n```\n");
        assert_eq!(explanations[0].claim, Claim::Differing);
    }

    #[test]
    fn a_fence_without_a_production_header_is_an_error() {
        let err = parse("```diff\n   'a' 'b'\n+  'c'\n```\n", "test.md").unwrap_err();
        assert!(err.to_string().contains("either heads the diff"));
    }

    #[test]
    fn a_diff_of_a_production_both_grammars_have_may_not_be_left_off() {
        let err = parse("```diff\nDesignFile =\n```\n", "test.md").unwrap_err();
        assert!(err.to_string().contains("heads no diff"));
    }

    #[test]
    fn an_unterminated_fence_is_an_error() {
        let err = parse("```diff\nA =\n+  'a'\n", "test.md").unwrap_err();
        assert!(err.to_string().contains("unterminated"));
    }

    #[test]
    fn normalize_drops_trailing_whitespace_but_keeps_the_diff_column() {
        assert_eq!(
            normalize(vec![
                String::new(),
                "-  A  ".to_owned(),
                "   B".to_owned(),
                "  ".to_owned(),
            ]),
            ["-  A", "   B"]
        );
    }
}
