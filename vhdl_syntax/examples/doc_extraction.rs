// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com
/// This is an example program showcasing how one can extract documentation comments
/// from an entity using the `vhdl_syntax` crate.
use std::collections::HashMap;
use vhdl_syntax::syntax::visitor::WalkEvent;
use vhdl_syntax::syntax::AstNode;
use vhdl_syntax::syntax::{DesignFileSyntax, EntityDeclarationSyntax};
use vhdl_syntax::tokens::{Trivia, TriviaPiece};

fn main() {
    let vhdl = "\
--- This is the foo entity
entity foo is
end foo;

-- Regular comment
--- This is the bar entity
entity bar is
end bar;
    ";
    let file = vhdl.parse::<DesignFileSyntax>().expect("Erroneous input");
    // HashMap containing the entity-names as keys and the associated doc-comment as values
    let mut comments = HashMap::new();
    // Walk the file filtering all entities.
    // Every walked AST node will be visited twice; once when entering and once when leaving.
    // Since we simply want to visit each entity, we only look at enter events.
    for entity in file.walk().filter_map(|event| match event {
        WalkEvent::Enter(node) => EntityDeclarationSyntax::cast(node),
        WalkEvent::Leave(_) => None,
    }) {
        // we check that there is an `entity` token.
        // Since the AST is capable of producing any input, no matter how erroneous,
        // all nodes and tokens are optional.
        if let Some(token) = entity.entity_token() {
            // The trivia is where all auxiliary information concerning a token is stored,
            // for example, comments, whitespaces or newlines.
            let comment = extract_doc_from_trivia(token.leading_trivia());
            // If the entity has an identifier, add the extracted documentation to the map
            if let Some(ident) = entity.identifier_token() {
                if !comment.is_empty() {
                    comments.insert(ident.text().to_string(), comment);
                }
            }
        }
    }

    assert_eq!(comments["foo"], "This is the foo entity");
    assert_eq!(comments["bar"], "This is the bar entity");
}

/// Extract documentation comments from one or more token's trivia.
/// Note that this is a toy example that simply checks for line comments that with a `-` (i.e.,
/// a doc comment is written using the `--- doc comment` syntax. A real-world example would probably
/// include more sophisticated processing.
fn extract_doc_from_trivia(trivia: Trivia) -> String {
    trivia
        .iter()
        .filter_map(|piece| match piece {
            TriviaPiece::LineComment(comment) => comment.strip_prefix("-"),
            _ => None,
        })
        .map(|str| str.trim())
        .collect()
}
