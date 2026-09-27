# Expressions

Expressions are among the most frequently used elements and must therefore be implemented efficiently and correctly.
Precedence (which operation is evaluated before which other, e.g., multiplication before addition) is of particular importance here.
The LRM solves this by defining the precedence via productions: an expression is a logical expression, which is a relation followed by 'and', 'or', 'xor', ... and another relation; a relation is a shift expression followed by '=', '/=', '<', ... and another shift expression, and so on:

```
Expression =
  ConditionOperator Primary
| LogicalExpression

LogicalExpression =
  Relation ('and' Relation)*
| Relation ('or' Relation)*
| ...

Relation =
  ShiftExpression (RelationalOperator ShiftExpression)?

ShiftExpression =
  SimpleExpression (ShiftOperator SimpleExpression)?

...
```

Translating this to `vhdl_syntax` would yield both an inefficient parser and deeply nested trees for simple (and common) expressions.
Consider, for example, an expression that consists of nothing but the number `1`:

```
Expression
  LogicalExpression
    Relation
      ShiftExpression
        SimpleExpression
          Term
            Factor
              Primary
                Literal
                  NumericLiteral
                    1
```

This shape is correct for the LRM, and it allows an elegant encoding of the precedence in the grammar.
`vhdl_syntax` takes a different approach: the precedence is enforced in the parser and the produced tree is lean.
The same expression produces the following tree:

```
Expression
  LiteralExpression
    1
```

## Basics

`Expression` in `vhdl_syntax` holds all basic expressions directly, rather than being the starting point of a nested cascade:

```diff
Expression =
-  ConditionOperator Primary
-| LogicalExpression
+  Literal
+| PhysicalLiteral
+| UnaryExpression
+| BinaryExpression
+| ParenthesizedExpressionOrAggregate
+| Allocator
+| Name
+| QualifiedExpression
```

Two expression kinds in `vhdl_syntax` replace the LRM cascade:

```diff
+ BinaryExpression
+ UnaryExpression
- Factor
- LogicalExpression
- Primary
- Relation
- ShiftExpression
- Term
- SimpleExpression
```

with their operators subsuming the LRM's individual operators:

```diff
+ BinaryOperator
+ UnaryOperator
- AddingOperator
- ConditionOperator
- LogicalOperator
- MiscellaneousOperator
- MultiplyingOperator
- RelationalOperator
- ShiftOperator
- Sign
```

`BinaryExpression` and `UnaryExpression` are defined as:

```
BinaryExpression =
  Lhs BinaryOperator Rhs

UnaryExpression =
  UnaryOperator Expression
```

with `Lhs` and `Rhs` being aliases of `Expression` that distinguish the left-hand side from the right-hand side of a binary expression:

```diff
+ Lhs
+ Rhs
```

## Allocator

The LRM's `Allocator` carries either a `SubtypeIndication` or a `QualifiedExpression`, which are almost indistinguishable:
parsing `new foo'bar(baz)` from the grammar alone does not yield an unambiguous result.
As part of the [name broadening strategy](./name.md), a plain `Name` can represent both forms, and the rest is handled by the `Expression` syntax:

```diff
Allocator =
-  'new' SubtypeIndication
-| 'new' QualifiedExpression
+  'new'
+  Expression
```

The qualified expression was broadened in a similar way, with `ParenthesizedExpressionOrAggregate` subsuming the two alternatives:

```diff
QualifiedExpression =
-  TypeMark ''' '(' Expression ')'
-| TypeMark ''' Aggregate
+  TypeMark
+  '''
+  ParenthesizedExpressionOrAggregate
```

## Choice

Similarly, every alternative of a choice except the `others` keyword is already covered by a plain `Expression`, which replaces `SimpleExpression`, `DiscreteRange` and `SimpleName`:

```diff
Choice =
-  SimpleExpression
-| DiscreteRange
-| SimpleName
+  Expression
 | 'others'
```

## Ranges

Ranges can be difficult to distinguish from plain expressions.
To simplify matters, a `BinaryExpression` can also denote a range: the `to` and `downto` keywords are folded into the `BinaryOperator`:

```
BinaryOperator =
  'and'
| 'or'
...
| 'to'
| 'downto'
```

Therefore, the LRM's `Range`, `DiscreteRange` and `Direction` no longer exist; they are entirely absorbed by `Expression`:

```diff
- Range
- DiscreteRange
- Direction
```

As a consequence, the productions that previously referenced them were updated to take a plain `Expression` instead:

```diff
RangeConstraint =
   'range'
-  Range
+  Expression
```

```diff
ParameterSpecification =
   '#identifier'
   'in'
-  DiscreteRange
+  Expression
```

```diff
IndexConstraint =
   '('
-  DiscreteRange
-  (',' DiscreteRange)*
+  Expression
+  (',' Expression)*
   ')'
```

## Aggregates

Since aggregates closely resemble expressions and cannot easily be distinguished from them, a new production, `ParenthesizedExpressionOrAggregate`, subsumes the two:

```diff
+ ParenthesizedExpressionOrAggregate
```

Note that in contexts where there is no ambiguity (currently only `Target`), an `Aggregate` is still used, unchanged from the LRM.

## Literals

Since `vhdl_syntax` can only represent homogeneous choices (the alternatives of a choice must either all be tokens or all be nodes), the `Literal` production only holds tokens:

```diff
Literal =
-  NumericLiteral
-| EnumerationLiteral
+  '#abstract_literal'
+| '#character_literal'
 | '#string_literal'
 | '#bit_string_literal'
 | 'null'
```

The `EnumerationLiteral` was inlined: its `'#character_literal'` alternative into `Literal` directly, its `'#identifier'` alternative through `Name`.
The production itself remains, since enumeration type definitions still use it.
The `NumericLiteral`, on the other hand, is gone entirely: its `'#abstract_literal'` alternative is a single token in `vhdl_syntax` and became an alternative of `Literal` directly.

```diff
- NumericLiteral
```

Its other alternative, `PhysicalLiteral`, is a node and could therefore not stay in `Literal`; it became a direct alternative of `Expression` instead:

```
Expression =
  Literal
| PhysicalLiteral
| UnaryExpression
| BinaryExpression
...
```

## Miscellaneous

Since `null` is an alternative of `Literal`, the `WaveformElement` production simplifies:

```diff
WaveformElement =
-  Expression ('after' Expression)?
-| 'null' ('after' Expression)?
+  Expression
+  ('after' Expression)?
```
