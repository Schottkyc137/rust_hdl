use vhdl_fmt::format;
use vhdl_syntax::parser::Parser;

fn format_str(code: &str) -> String {
    let mut parser = Parser::new(code.as_bytes().into());
    parser.block_statement();
    let (node, diagnostics) = parser.into_root();
    assert!(diagnostics.is_empty());
    format(node).to_string()
}

#[test]
fn block_statement() {
    insta::assert_snapshot!(format_str("block is begin end block ;"));
}
