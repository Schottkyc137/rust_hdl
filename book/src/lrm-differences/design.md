# Design

```diff
DesignFile =
   DesignUnit
   DesignUnit*
+  '#eof'
```

A design file explicitly contains an EOF (end-of-file) token.
Since any token in `vhdl_syntax` [only carries its leading trivia](../nodes-and-tokens/syntax-tokens.md#trivia), the EOF token is what carries the trivia at the end of the file — trailing newlines, or a comment after the last design unit — which would otherwise have no token to attach to.

## Block Specification

```diff
BlockSpecification =
   Name
-| Label ('(' GenerateSpecification ')')?
```

The LRM's second form is indistinguishable from a `Name`, therefore it is folded into one.
