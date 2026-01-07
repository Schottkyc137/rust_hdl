use vhdl_fmt::format;
use vhdl_syntax::{parser, syntax::AstNode};

fn format_str(code: &str) -> String {
    let (node, diagnostics) = parser::parse(code.as_bytes());
    assert!(diagnostics.is_empty());
    format(node.raw()).to_string()
}

#[test]
fn empty_architecture() {
    insta::assert_snapshot!(format_str(
        "architecture  arch  of  foo  is  begin  end  arch ;"
    ));
    insta::assert_snapshot!(format_str(
        "architecture  arch  of  foo  is  begin  end  architecture  arch ;"
    ));
}

#[test]
fn architecture_with_declarations() {
    insta::assert_snapshot!(format_str(
        "architecture  arch  of  foo  is  signal foo: bit;  begin  end  arch ;"
    ));
}

#[test]
fn architecture_with_statmeents() {
    insta::assert_snapshot!(format_str(
        "architecture  arch  of  foo  is  begin foo <= '1';  end  arch ;"
    ));
}

#[test]
fn architecture_with_statmeents_and_declarations() {
    insta::assert_snapshot!(format_str(
        "architecture  arch  of  foo  is signal foo: bit;  begin x <= '1';  end  arch ;"
    ));
}
