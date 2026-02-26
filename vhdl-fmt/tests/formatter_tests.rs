// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2026, Lukas Scheller lukasscheller@icloud.com

//! Integration tests for the `vhdl-fmt` formatter.
//!
//! All test inputs are built programmatically with the `vhdl_syntax` builder
//! API. Tests cover the formatting capabilities of the formatter as-is; no
//! bugs are fixed here. Tests for known-buggy behavior are marked `#[ignore]`
//! with an explanation.

use vhdl_fmt::{
    config::{Config, IndentStyle, Indentation, NewlineStyle},
    format, format_with_config,
};
use vhdl_syntax::{
    parser,
    syntax::{
        AstNode, ConcurrentStatementSyntax, DeclarationSyntax, LibraryUnitSyntax,
        NameDesignatorToken, NamePrefixSyntax, NameSyntax, PrimaryUnitSyntax,
        ResolutionIndicationSyntax, SecondaryUnitSyntax, builders::*,
    },
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Build a simple identifier name node.
fn simple_name(ident: &[u8]) -> NameSyntax {
    NameBuilder::new(NamePrefixSyntax::NameDesignatorPrefix(
        NameDesignatorPrefixBuilder::new(NameDesignatorToken::identifier(ident)).build(),
    ))
    .build()
}

/// Format a syntax node with default config and return the resulting string.
fn fmt(node: impl AstNode) -> String {
    format(node.raw()).to_string()
}

// ---------------------------------------------------------------------------
// Entity Declaration
// ---------------------------------------------------------------------------

// NOTE: Currently, most testcases that originate from the builder leave a leading space.
// This is expected currently and is due to the fact that we use verbatim trivia in places that we
// do not format.
// Once we format all tokens and remove the verbatim trivia, this must be removed

/// A minimal entity (`entity my_entity is end ;`) is formatted with
/// newlines before the preamble and epilogue nodes, and no indentation.
#[test]
fn empty_entity_formatting() {
    let entity =
        EntityDeclarationBuilder::new(EntityDeclarationPreambleBuilder::new(b"my_entity")).build();
    assert_eq!(fmt(entity), " entity my_entity is\nend ;");
}

/// The optional identifier in the epilogue is preserved and formatted
/// on the same line as `end`.
#[test]
fn entity_formatting_with_epilogue_name() {
    let entity = EntityDeclarationBuilder::new(EntityDeclarationPreambleBuilder::new(b"my_entity"))
        .with_entity_declaration_epilogue(
            EntityDeclarationEpilogueBuilder::new().with_identifier_token(b"my_entity"),
        )
        .build();
    assert_eq!(fmt(entity), " entity my_entity is\nend my_entity ;");
}

/// The optional `entity` keyword in the epilogue is included when set.
#[test]
fn entity_formatting_with_full_epilogue() {
    use vhdl_syntax::tokens::{Keyword as Kw, Token, TokenKind, Trivia, TriviaPiece};
    let entity_kw = Token::new(
        TokenKind::Keyword(Kw::Entity),
        Kw::Entity.canonical_text(),
        Trivia::from([TriviaPiece::Spaces(1)]),
    );
    let entity = EntityDeclarationBuilder::new(EntityDeclarationPreambleBuilder::new(b"my_entity"))
        .with_entity_declaration_epilogue(
            EntityDeclarationEpilogueBuilder::new()
                .with_entity_token(entity_kw)
                .with_identifier_token(b"my_entity"),
        )
        .build();
    assert_eq!(fmt(entity), " entity my_entity is\nend entity my_entity ;");
}

// ---------------------------------------------------------------------------
// Architecture Body
// ---------------------------------------------------------------------------

/// A minimal architecture (`architecture rtl of my_entity is begin end ;`)
/// is formatted with newlines before preamble, `begin`, and epilogue.
#[test]
fn empty_architecture_formatting() {
    let arch = ArchitectureBodyBuilder::new(ArchitecturePreambleBuilder::new(
        b"rtl",
        simple_name(b"my_entity"),
    ))
    .build();
    assert_eq!(fmt(arch), " architecture rtl of my_entity is\nbegin\nend ;");
}

/// Declarations inside an architecture body are indented by one level (4
/// spaces by default). A `UseClauseDeclaration` is used here because it
/// can be built without running into the `SubtypeIndicationBuilder` bug
/// (that builder incorrectly requires `resolution_indication` as a required
/// argument even though VHDL makes it optional).
#[test]
fn architecture_declarations_are_indented() {
    let use_decl =
        DeclarationSyntax::UseClauseDeclaration(UseClauseDeclarationBuilder::default().build());
    let declarations = DeclarationsBuilder::new()
        .add_declarations(use_decl)
        .build();
    let arch = ArchitectureBodyBuilder::new(ArchitecturePreambleBuilder::new(
        b"rtl",
        simple_name(b"my_entity"),
    ))
    .with_declarations(declarations)
    .build();
    assert_eq!(
        fmt(arch),
        " architecture rtl of my_entity is\n    use ;\nbegin\nend ;"
    );
}

/// Concurrent statements inside an architecture body are indented.
/// A process statement (with mandatory label) is used as the concurrent
/// statement.
///
/// Note: `ProcessStatementBuilder` stores the process body as
/// `ConcurrentStatements` instead of `SequentialStatements` — a model bug.
/// No body statements are added here to avoid exercising that bug.
#[test]
fn architecture_concurrent_statements_are_indented() {
    let label = LabelBuilder::new(b"p1").build();
    let preamble = ProcessStatementPreambleBuilder::new(label).build();
    let process = ProcessStatementBuilder::new(preamble).build();
    let concurrent_stmts = ConcurrentStatementsBuilder::new()
        .add_concurrent_statements(ConcurrentStatementSyntax::ProcessStatement(process))
        .build();
    let arch = ArchitectureBodyBuilder::new(ArchitecturePreambleBuilder::new(
        b"rtl",
        simple_name(b"my_entity"),
    ))
    .with_concurrent_statements(concurrent_stmts)
    .build();
    assert_eq!(
        fmt(arch),
        " architecture rtl of my_entity is\nbegin\n    p1 : process\n    begin\n    end process ;\nend ;"
    );
}

// ---------------------------------------------------------------------------
// Design File (multiple design units)
// ---------------------------------------------------------------------------

/// A design file containing an entity followed by an architecture is
/// formatted with newlines between design units. The EOF token retains its
/// default leading space trivia because `BreakKind::Unset` leaves trivia
/// unchanged (trailing space after the last `;`).
#[test]
fn design_file_entity_and_architecture() {
    let entity =
        EntityDeclarationBuilder::new(EntityDeclarationPreambleBuilder::new(b"foo")).build();
    let arch = ArchitectureBodyBuilder::new(ArchitecturePreambleBuilder::new(
        b"rtl",
        simple_name(b"foo"),
    ))
    .build();
    let file = DesignFileBuilder::new()
        .add_design_units(DesignUnitBuilder::new(LibraryUnitSyntax::PrimaryUnit(
            PrimaryUnitSyntax::EntityDeclaration(entity),
        )))
        .add_design_units(DesignUnitBuilder::new(LibraryUnitSyntax::SecondaryUnit(
            SecondaryUnitSyntax::ArchitectureBody(arch),
        )))
        .build();
    // The trailing " " comes from the EOF token whose trivia (Spaces(1) from
    // canonical_token()) is preserved as-is when BreakKind::Unset applies.
    assert_eq!(
        fmt(file),
        " entity foo is\nend ;\narchitecture rtl of foo is\nbegin\nend ; "
    );
}

// ---------------------------------------------------------------------------
// Formatter configuration
// ---------------------------------------------------------------------------

/// When `NewlineStyle::CarriageReturnLineFeed` is configured, every newline
/// is emitted as `\r\n`.
#[test]
fn crlf_newline_style() {
    let entity =
        EntityDeclarationBuilder::new(EntityDeclarationPreambleBuilder::new(b"my_entity")).build();
    let config = Config {
        newline_style: NewlineStyle::CarriageReturnLineFeed,
        ..Config::default()
    };
    let formatted = format_with_config(entity.raw(), config).to_string();
    assert_eq!(formatted, " entity my_entity is\r\nend ;");
}

/// Indentation width is configurable. Here 2-space indentation is verified
/// using the architecture-with-declarations case.
#[test]
fn two_space_indentation() {
    let use_decl =
        DeclarationSyntax::UseClauseDeclaration(UseClauseDeclarationBuilder::default().build());
    let declarations = DeclarationsBuilder::new()
        .add_declarations(use_decl)
        .build();
    let arch = ArchitectureBodyBuilder::new(ArchitecturePreambleBuilder::new(
        b"rtl",
        simple_name(b"my_entity"),
    ))
    .with_declarations(declarations)
    .build();
    let config = Config {
        indentation: Indentation {
            style: IndentStyle::Spaces,
            width: 2,
        },
        ..Config::default()
    };
    let formatted = format_with_config(arch.raw(), config).to_string();
    assert_eq!(
        formatted,
        " architecture rtl of my_entity is\n  use ;\nbegin\nend ;"
    );
}

/// Tab indentation style is configurable.
#[test]
fn tab_indentation() {
    let use_decl =
        DeclarationSyntax::UseClauseDeclaration(UseClauseDeclarationBuilder::default().build());
    let declarations = DeclarationsBuilder::new()
        .add_declarations(use_decl)
        .build();
    let arch = ArchitectureBodyBuilder::new(ArchitecturePreambleBuilder::new(
        b"rtl",
        simple_name(b"my_entity"),
    ))
    .with_declarations(declarations)
    .build();
    let config = Config {
        indentation: Indentation {
            style: IndentStyle::Tabs,
            width: 1,
        },
        ..Config::default()
    };
    let formatted = format_with_config(arch.raw(), config).to_string();
    assert_eq!(
        formatted,
        " architecture rtl of my_entity is\n\tuse ;\nbegin\nend ;"
    );
}

// ---------------------------------------------------------------------------
// Signal declaration (known bug in builder model)
// ---------------------------------------------------------------------------

/// Signal declarations cannot be cleanly built because `SubtypeIndicationBuilder`
/// requires `resolution_indication` as a mandatory argument even though VHDL
/// defines it as optional. Additionally, `IdentifierListBuilder` always emits
/// a comma token even for a single-identifier list.
///
/// This test is ignored until the builder model is fixed.
#[test]
#[ignore = "SubtypeIndicationBuilder requires resolution_indication (optional in VHDL) and \
            IdentifierListBuilder always emits a trailing comma"]
fn signal_declaration_indentation() {
    // Workaround: provide the same name for both resolution_indication and
    // type_mark. The resulting subtype_indication text is "std_logic std_logic"
    // which is semantically invalid but exercises the formatter's indentation.
    let type_name = simple_name(b"std_logic");
    let resolution_name = simple_name(b"std_logic");
    let subtype = SubtypeIndicationBuilder::new(
        ResolutionIndicationSyntax::NameResolutionIndication(
            NameResolutionIndicationBuilder::new(resolution_name).build(),
        ),
        type_name,
    )
    .build();
    let id_list = IdentifierListBuilder::new(b"clk").build();
    let signal = DeclarationSyntax::SignalDeclaration(
        SignalDeclarationBuilder::new(id_list, subtype).build(),
    );
    let declarations = DeclarationsBuilder::new().add_declarations(signal).build();
    let arch = ArchitectureBodyBuilder::new(ArchitecturePreambleBuilder::new(
        b"rtl",
        simple_name(b"my_entity"),
    ))
    .with_declarations(declarations)
    .build();
    // Expected (assuming bugs were fixed): "\narchitecture rtl of my_entity is\n    signal clk : std_logic ;\nbegin\nend ;"
    let _ = fmt(arch);
}

// ---------------------------------------------------------------------------
// Blank line preservation (user-supplied blank lines)
// ---------------------------------------------------------------------------

/// Helper: parse VHDL source and format it with default config.
fn fmt_str(src: &str) -> String {
    let (design_file, _) = parser::parse(src);
    format(design_file.raw()).to_string()
}

/// A single blank line between two use clauses inside an architecture body is
/// preserved in the formatted output.
#[test]
fn single_blank_line_preserved_between_declarations() {
    let src = "architecture rtl of my_entity is\n    use work.pkg1.all;\n\n    use work.pkg2.all;\nbegin\nend ;";
    let result = fmt_str(src);
    // Expect two newlines between the use clauses (one for the newline, one for the blank).
    // The formatter preserves the original semicolon trivia (no space before it in source).
    assert!(
        result.contains("use work.pkg1.all;\n\n    use work.pkg2.all;"),
        "Expected single blank line preserved, got:\n{result}"
    );
}

/// Multiple user-supplied blank lines between declarations are preserved.
#[test]
fn multiple_blank_lines_preserved_between_declarations() {
    let src = "architecture rtl of my_entity is\n    use work.pkg1.all;\n\n\n\n    use work.pkg2.all;\nbegin\nend ;";
    let result = fmt_str(src);
    // Three blank lines (four newlines total) should survive.
    assert!(
        result.contains("use work.pkg1.all;\n\n\n\n    use work.pkg2.all;"),
        "Expected three blank lines preserved, got:\n{result}"
    );
}

/// A blank line between two line comments preceding a token is preserved.
#[test]
fn blank_line_between_comments_preserved() {
    let src = "architecture rtl of my_entity is\n    -- foo\n\n    -- bar\n    use work.pkg.all;\nbegin\nend ;";
    let result = fmt_str(src);
    // The blank line between the two comments should survive.
    // The second comment gets the formatter-applied indentation (4 spaces).
    assert!(
        result.contains("-- foo\n\n    -- bar"),
        "Expected blank line between comments, got:\n{result}"
    );
}

/// A line comment followed by a blank line before a `wants_newline_before` node
/// (here a use-clause declaration) must preserve the blank line and must NOT
/// insert a spurious extra newline.
///
/// Previously, `pending_break` (set by `wants_newline_before`) was always pushed
/// after the comment trivia loop, producing two consecutive `HardBreak` nodes for
/// the same boundary (comment → token). The fix drops `pending_break` when the
/// trivia loop already emitted a hard trailing separator, so there is exactly one
/// `HardBreak` per boundary and the blank line is preserved correctly.
#[test]
fn comment_blank_line_before_wants_newline_before_node() {
    // One blank line between the comment and the use clause.
    let src = "architecture rtl of my_entity is\n    -- section header\n\n    use work.pkg.all;\nbegin\nend ;";
    let result = fmt_str(src);
    // The blank line between the comment and the use clause must be preserved.
    assert!(
        result.contains("-- section header\n\n    use work.pkg.all;"),
        "Expected blank line after comment preserved, got:\n{result}"
    );
    // There must be no triple newline — the blank line must not be doubled.
    assert!(
        !result.contains("-- section header\n\n\n"),
        "Unexpected extra newline after comment, got:\n{result}"
    );
}

/// No blank lines appear where the formatter emits a space (e.g., tokens on
/// the same line in flat-layout mode, such as inside a short interface list).
/// This verifies that `Doc::Space` overrules any stale blank-lines hint.
#[test]
fn space_overrules_blank_lines() {
    // A minimal entity with a generic clause that will fit on one line.
    // The blank lines in the source trivia must not leak into the formatted output.
    let src = "entity my_entity is\n    generic ( G : integer ) ;\nend ;";
    let result = fmt_str(src);
    // There should be no double newlines anywhere inside the generic clause line.
    assert!(
        !result.contains("(\n\n"),
        "Unexpected blank line inside flat-layout group, got:\n{result}"
    );
}

// ---------------------------------------------------------------------------
// Colon alignment
// ---------------------------------------------------------------------------

/// Two ports with different identifier lengths in a port clause produce aligned
/// colons: the shorter identifier is padded with spaces so both colons sit in
/// the same column.
#[test]
fn colon_alignment_in_interface_list() {
    // clk_i (5 chars) and data_output_bus (15 chars).
    // max_width = 15.
    // clk_i       → spaces = 15 − 5 + 1 = 11
    // data_output_bus → spaces = 15 − 15 + 1 = 1
    let src = "entity my_entity is\n    port (\n        clk_i : in std_logic;\n        data_output_bus : in std_logic_vector(15 downto 0)\n    );\nend;";
    let result = fmt_str(src);
    assert!(
        result.contains("clk_i           :"),
        "Expected 'clk_i' with 11 alignment spaces before ':', got:\n{result}"
    );
    assert!(
        result.contains("data_output_bus :"),
        "Expected 'data_output_bus' with 1 space before ':', got:\n{result}"
    );
}

/// A single-item interface list has no alignment partner; the colon still
/// receives exactly 1 space (max_width == identifier width, so spaces = 1).
#[test]
fn colon_alignment_single_item_no_extra_padding() {
    // G has width 1; max_width = 1; spaces = 1 − 1 + 1 = 1.
    let src = "entity my_entity is\n    generic (\n        G : integer\n    );\nend;";
    let result = fmt_str(src);
    assert!(
        result.contains("G :"),
        "Expected 'G :' with exactly 1 space before ':', got:\n{result}"
    );
    assert!(
        !result.contains("G  :"),
        "Expected no extra padding before ':' in single-item list, got:\n{result}"
    );
}

/// Signal declarations with different identifier lengths inside an architecture
/// body produce aligned colons.
#[test]
#[ignore = "Column alignment in declarations not yet implemented"]
fn colon_alignment_in_declarations() {
    // Tokens before ':' for each signal:
    //   signal clk     → "signal"(6) + "clk"(3) = width 10
    //   signal data_out → "signal"(6) + "data_out"(8) = width 15
    // max_width = 15.
    // clk     → spaces = 15 − 10 + 1 = 6
    // data_out → spaces = 15 − 15 + 1 = 1
    let src = "architecture rtl of my_entity is\n    signal clk : std_logic;\n    signal data_out : std_logic_vector(7 downto 0);\nbegin\nend;";
    let result = fmt_str(src);
    assert!(
        result.contains("signal clk      :"),
        "Expected 'signal clk' with 6 alignment spaces before ':', got:\n{result}"
    );
    assert!(
        result.contains("signal data_out :"),
        "Expected 'signal data_out' with 1 space before ':', got:\n{result}"
    );
}

/// Record element declarations with different field name lengths produce
/// aligned colons.
#[test]
fn colon_alignment_in_record_elements() {
    // x       → width 1; y_coord → width 7; max_width = 7.
    // x       → spaces = 7 − 1 + 1 = 7
    // y_coord → spaces = 7 − 7 + 1 = 1
    let src = "package pkg is\n    type my_record is record\n        x : integer;\n        y_coord : std_logic;\n    end record;\nend package;";
    let result = fmt_str(src);
    assert!(
        result.contains("x       :"),
        "Expected 'x' with 7 alignment spaces before ':', got:\n{result}"
    );
    assert!(
        result.contains("y_coord :"),
        "Expected 'y_coord' with 1 space before ':', got:\n{result}"
    );
}

#[test]
fn design_entities_only_have_explicit_top_level_newlines() {
    // No newline before entity -> don't want that in the output
    let formatted = fmt_str(
        "\
entity foo is
end;",
    );
    insta::assert_snapshot!(formatted);

    // Newline before entity -> preserve
    let formatted = fmt_str(
        "
entity foo is
end;",
    );
    insta::assert_snapshot!(formatted);
}

#[test]
fn consecutive_declarations_do_not_influence_formatting_of_each_other() {
    // There was a bug where the comment after signal a was emitted to the next line.
    // If signal b is omitted, the comment is trailing (like it should be)
    let formatted = fmt_str(
        "\
architecture foo of bar is
  signal a : bit; -- Comment
  signal b : std_logic;
begin
end foo;
",
    );
    insta::assert_snapshot!(formatted);
}

/// A trailing line comment in a record type definition is always hoisted to
/// appear *before* the element it was on, because record element declarations
/// always use broken (non-flat) layout.
#[test]
fn trailing_comment_in_record_is_hoisted_before_element() {
    let formatted = fmt_str(
        "\
package pkg is
  type my_record is record
    field_a : std_logic; -- describes field_a
    field_b : integer;
  end record;
end pkg;
",
    );
    insta::assert_snapshot!(formatted);
}

/// A trailing line comment inside a port list is always hoisted before the
/// port it annotated, regardless of port-name length.  A trailing *line*
/// comment always introduces a newline, which makes the interface-list group
/// flat_width = None (always broken), so hoisting unconditionally applies.
#[test]
fn trailing_comment_in_interface_list_is_hoisted() {
    let formatted = fmt_str(
        "\
entity foo is
  port (very_long_signal_name_here : in std_logic_vector(7 downto 0); -- hoisted
        another_very_long_signal : out std_logic);
end foo;
",
    );
    insta::assert_snapshot!(formatted);
}

#[test]
fn trailing_comment_is_hoisted_before_last_other_comment() {
    let formatted = fmt_str(
        "\
package pkg is
  type my_record is record
    -- other comment
    field_a : std_logic; -- describes field_a
    field_b : integer;
  end record;
end pkg;
",
    );
    insta::assert_snapshot!(formatted);
}

// #[test]
// fn does_not_split_into_multiple_lines_for_elements_shorter_than_the_maximum_column_width() {
//     let formatted = fmt_str(
//         "\
// package foo is
//     function f(short_arg : std_logic);
// end;
// ",
//     );
//     insta::assert_snapshot!(formatted);
// }
