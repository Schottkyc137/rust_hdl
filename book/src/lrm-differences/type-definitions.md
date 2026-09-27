# Type Definitions

In the LRM, `IntegerTypeDefinition` and `FloatingTypeDefinition` are both aliases for `RangeConstraint`.
Since the two aliases would clash if copied verbatim into the modified ungrammar, a single `RangeConstraint` alternative is used instead.

```diff
- FloatingTypeDefinition
- IntegerTypeDefinition
```

```diff
ScalarTypeDefinition =
   EnumerationTypeDefinition
-| IntegerTypeDefinition
-| FloatingTypeDefinition
+| RangeConstraint
 | PhysicalTypeDefinition
```
