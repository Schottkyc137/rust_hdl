# SyntaxTokens

## Trivia

Trivia is anything that is not relevant for parsing VHDL code, like whitespace, comments, newlines, etc.
A traditional compiler usually throws trivia away at the first moment it can, but the aim of `vhdl_syntax` is to represent VHDL code in a lossless manner.
This necessitates keeping the trivia in the parsed tree.

Here, trivia is attached to a token, more specifically, to the leading edge of a token.
Consider the following example that defines two tokens (`entity` and `entity_name`) and trivia that comprises four spaces between them:

```
entity    entity_name
|----|----|---------|
  A    WS      B
```

`vhdl_syntax` attaches the four spaces between `entity` and `entity_name` to the `entity_name` token:

```
'entity'
  leading trivia = []

'entity_name'
  leading trivia = [Whitespace(4)]
```
<!-- TODO: add a little example later -->

In `vhdl_syntax`, trivia is modeled as a collection of `TriviaPiece`s, roughly equivalent to the following `struct`:

```rust,no_run,noplayground
struct Trivia {
    pieces: Vec<TriviaPiece>
}
```

where each `TriviaPiece` is one of the following cases:

```rust,no_run,noplayground
{{#include ../../../vhdl_syntax/src/tokens/trivia_piece.rs:trivia-piece}}
```

### Comments

TODO: Work in progress
