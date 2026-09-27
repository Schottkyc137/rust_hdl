# Name

LRM-faithful names are among the hardest syntactic elements to parse.
For example, parsing
```vhdl
foo(bar)
```
as a name could yield either

- an `IndexedName`, with `bar` being the index, or
- a `SliceName`, with `bar` being a `DiscreteRange`.

The LRM deals with these ambiguities by requiring analysis-level information. For example:

```
// Transcribed from the LRM directly, not from vhdl-08.ungram
ActualDesignator =
  signal_Name
| variable_Name
| file_Name
| subprogram_Name
| instantiated_package_Name
...
```

i.e., the differentiation happens based on whether the name denotes a signal, a variable, a file, and so on.

This works for a compiler: to analyze, elaborate and run a design, it must know all of that information anyway, and if it is missing, it reports an error and bails.
However, it is not a good shape for a language server or a generic syntax tool, though:

- For tasks like formatting, simple lint rules, or simple testbench extractors, that information is not required.
- For error-tolerant tasks like a language server, the information might not be there at all, be it because of incorrect input, missing libraries, or a toolchain that is not set up correctly.

In all of these cases, one still wants parsing to continue normally rather than aborting completely.
The general strategy employed by `vhdl_syntax` is broadening: the new `Name` syntax accepts more than the LRM allows.
Sorting out the invalid variants is then the task of a subsequent analysis stage.

## Basics

The revised name production comprises a `Prefix`, a number of `NameTail`s and an optional `RangeConstraint`:

```diff
Name =
-  SimpleName
-| OperatorSymbol
-| '#character_literal'
-| SelectedName
-| IndexedName
-| SliceName
-| AttributeName
-| ExternalName
+  Prefix
+  NameTail*
+  RangeConstraint?
```

Unlike in the LRM, the `Prefix` does not recurse, it is simply the start of a name:

```diff
Prefix =
-  Name
-| FunctionCall
+  ExternalName
+| NameDesignator
```

A `NameDesignator` takes the place of the `SimpleName`, `OperatorSymbol` and character literal alternatives that the LRM's `Name` starts with:

```diff
+ NameDesignator
```

Since a `SelectedName` is no longer a complete name of its own (see below), every site that used one was broadened to accept any `Name`:

```diff
UseClause =
   'use'
-  SelectedName
-  (',' SelectedName)*
+  Name
+  (',' Name)*
   ';'
```

```diff
ContextReference =
   'context'
-  SelectedName
-  (',' SelectedName)*
+  Name
+  (',' Name)*
   ';'
```

The addition of the `RangeConstraint` at the end is due to an ambiguity with the `SubtypeIndication`; read more in the [dedicated section](./subtype-indication.md).

## Name Tails

The `NameTail` replaces and simplifies the LRM's nested `SelectedName` / `IndexedName` / `SliceName` / ... structure:

```diff
+ NameTail
```

Two of the LRM's name productions carry over as name tails with only the `Prefix` removed.
The first one is the `SelectedName`:

```diff
SelectedName =
-  Prefix
   '.'
   Suffix
```

For the second one, the `AttributeName`, the trailing `Expression` was dropped as well, since it is ambiguous with a trailing association list:

```diff
AttributeName =
-  Prefix
   Signature?
   '''
   AttributeDesignator
-  ('(' Expression ')')?
```

The `AttributeDesignator` was broadened to accept the reserved words `range` and `subtype`, which are valid attribute designators even though they are not simple names:

```diff
AttributeDesignator =
-  SimpleName
+  '#identifier'
+| 'range'
+| 'subtype'
```

Since an attribute specification can never spell one of those reserved words, it accepts an identifier directly rather than an `AttributeDesignator`:

```diff
AttributeSpecification =
   'attribute'
-  AttributeDesignator
+  '#identifier'
   'of'
   EntitySpecification
   'is'
   Expression
   ';'
```

The `IndexedName` and `SliceName` are merged into a `ParenthesizedName`:

```diff
- IndexedName
- SliceName
+ ParenthesizedName
```

## External Names

External names stay largely the same as the LRM describes them; the differences are in the individual path names.
It is impractical for a `PackagePathname` to distinguish a leading identifier from a list of repeated identifiers and a trailing identifier, therefore the syntax allows an arbitrary list of identifiers instead.
Mapping those onto the respective elements is left to the later analysis step.

```diff
PackagePathname =
   '@'
-  LogicalName
-  '.'
-  (SimpleName '.')*
-  SimpleName
+  '#identifier'
+  ('.' '#identifier')*
```

Similarly, it is impractical to single out the trailing `SimpleName` of a `PartialPathname`.
Instead, the production is a plain list of `PathnameElement`s:

```diff
PartialPathname =
-  (PathnameElement '.')*
-  SimpleName
+  PathnameElement
+  ('.' PathnameElement)*
```

For simplicity, the `PathnameElement`'s two alternatives were merged into one:

```diff
PathnameElement =
-  SimpleName
-| Label ('(' Expression ')')?
+  '#identifier'
+  ('(' Expression ')')?
```

## Formal and Actual Parts

Actual parts are the elements of the association lists in function calls, procedure calls and instantiations.
As part of the name broadening strategy, these are merged into the `ParenthesizedName`, which is just a parenthesized `AssociationList`:

```
ParenthesizedName =
  '(' AssociationList ')'
```

The `ActualPart` has been broadened, with the `ActualPartBody` carrying an `Expression`, a `SubtypeIndication` or the `open` keyword:

```diff
ActualPart =
-  ActualDesignator
-| Name '(' ActualDesignator ')'
-| TypeMark '(' ActualDesignator ')'
+  'inertial'?
+  ActualPartBody
```

```diff
+ ActualPartBody
```

A formal part is completely described by a `Name`, therefore it was folded into one:

```diff
FormalPart =
-  FormalDesignator
-| Name '(' FormalDesignator ')'
-| TypeMark '(' FormalDesignator ')'
+  Name
```

The `FormalDesignator` was removed entirely:

```diff
- FormalDesignator
```

The LRM's `ActualDesignator` and `ActualParameterPart` are subsumed by the changes above:

```diff
- ActualDesignator
- ActualParameterPart
```

## Miscellaneous

Several productions end with a parenthesized part after a name that cannot be distinguished from a plain name.
In the modified grammar, that parenthesized part was removed, since the `Name` already covers it:

```diff
GroupDeclaration =
   'group'
   '#identifier'
   ':'
   Name
-  '('
-  GroupConstituentList
-  ')'
   ';'
```

```diff
- GroupConstituent
- GroupConstituentList
```

```diff
InstantiatedUnit =
   'component'? Name
-| 'entity' Name ('(' '#identifier' ')')?
+| 'entity' Name
 | 'configuration' Name
```

```diff
EntityAspect =
-  'entity' Name ('(' '#identifier' ')')?
+  'entity' Name
 | 'configuration' Name
 | 'open'
```

```diff
ProcedureCall =
   Name
-  ('(' ActualParameterPart ')')?
```

Several productions that cannot be distinguished from a name were removed altogether:

```diff
- FunctionCall
- GenerateSpecification
- TypeConversion
```
