# AST Syntax gen

This crate is used to generate rust code for the AST nodes in `vhdl_syntax`.

## Re-generating nodes

Change directory to `vhdl_syntax/src/syntax/generated` and run

```sh
cargo run --bin syntax-gen --manifest-path ../../../../ast_syntax_gen/Cargo.toml
```
