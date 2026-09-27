# Differences from the Language Reference Manual (LRM)

A grammar that faithfully follows the LRM cannot be used to build a purely syntactic parser.
This chapter describes the differences from the LRM and the rationale behind them.
Both the [original grammar](https://github.com/VHDL-LS/rust_hdl/blob/master/xtask/doc/vhdl-08.ungram) and the [modified grammar](https://github.com/VHDL-LS/rust_hdl/blob/master/xtask/doc/vhdl-08-modified.ungram), written in the [ungrammar](https://rust-analyzer.github.io/blog/2020/10/24/introducing-ungrammar.html) language, can be found in the GitHub repository.

## How to read this chapter

Differences from the LRM are shown as diffs:

```diff
Production =
-  Bar
+  Foo
   Baz
```

This syntax means that `Production` has the LRM's `Bar` node replaced by `Foo`, i.e., the LRM writes

```
Production =
  Bar
  Baz
```

while `vhdl_syntax` writes

```
Production =
  Foo
  Baz
```

If a whole production is new in `vhdl_syntax`, its name is prefixed with a `+`.
If a production was not carried over from the LRM, its name is prefixed with a `-`:

```diff
+ BinaryExpression
- Term
```

The example above means that `vhdl_syntax` defines a `BinaryExpression` production, but omits the LRM's `Term` production.
