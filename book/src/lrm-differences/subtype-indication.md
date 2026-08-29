# Subtype Indication

The constraint that the LRM permits after an arbitrary `Name` (`TypeMark` is just an alias for a name) can be indistinguishable from a `ParenthesizedName`.
Therefore, the `Constraint` was merged into the name.
The `RangeConstraint` is the only constraint that can still be parsed unambiguously, since it is introduced by the `range` keyword, so it was attached to the `Name` as a trailing element.
Every other constraint is parenthesized and is therefore already covered by a name tail.

```diff
SubtypeIndication =
   ResolutionIndication?
   TypeMark
-  Constraint?
```

Those other constraints are completely absorbed by the name and were removed:

```diff
- ArrayConstraint
- ArrayElementConstraint
- Constraint
- ElementConstraint
- RecordConstraint
- RecordElementConstraint
```
