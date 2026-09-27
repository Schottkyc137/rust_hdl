# Statements

Statements mostly carry over verbatim from the LRM.
The main difference is the placement of the `Label`: the LRM usually attaches it to a parent production, whereas `vhdl_syntax` places it on the node it belongs to, for simplicity and to avoid deeply nested nodes.

## Concurrent Signal Assignment Statements

```diff
ConcurrentSignalAssignmentStatement =
-  (Label ':')? 'postponed'? ConcurrentSimpleSignalAssignment
-| (Label ':')? 'postponed'? ConcurrentConditionalSignalAssignment
-| (Label ':')? 'postponed'? ConcurrentSelectedSignalAssignment
+  ConcurrentSimpleSignalAssignment
+| ConcurrentConditionalSignalAssignment
+| ConcurrentSelectedSignalAssignment
```

```diff
ConcurrentSimpleSignalAssignment =
+  (Label ':')?
+  'postponed'?
   Target
   '<='
   'guarded'?
   DelayMechanism?
   Waveform
   ';'
```

```diff
ConcurrentConditionalSignalAssignment =
+  (Label ':')?
+  'postponed'?
   Target
   '<='
   'guarded'?
   DelayMechanism?
   ConditionalWaveforms
   ';'
```

```diff
ConcurrentSelectedSignalAssignment =
+  (Label ':')?
+  'postponed'?
   'with'
   Expression
   'select'
   '?'?
   Target
   '<='
   'guarded'?
   DelayMechanism?
   SelectedWaveforms
   ';'
```

## Signal Assignment Statements

```diff
SignalAssignmentStatement =
-  (Label ':')? SimpleSignalAssignment
-| (Label ':')? ConditionalSignalAssignment
-| (Label ':')? SelectedSignalAssignment
+  SimpleSignalAssignment
+| ConditionalSignalAssignment
+| SelectedSignalAssignment
```

```diff
SimpleWaveformAssignment =
+  (Label ':')?
   Target
   '<='
   DelayMechanism?
   Waveform
   ';'
```

```diff
ConditionalWaveformAssignment =
+  (Label ':')?
   Target
   '<='
   DelayMechanism?
   ConditionalWaveforms
   ';'
```

```diff
SimpleForceAssignment =
+  (Label ':')?
   Target
   '<='
   'force'
   ForceMode?
   Expression
   ';'
```

```diff
ConditionalForceAssignment =
+  (Label ':')?
   Target
   '<='
   'force'
   ForceMode?
   ConditionalExpressions
   ';'
```

```diff
SimpleReleaseAssignment =
+  (Label ':')?
   Target
   '<='
   'release'
   ForceMode?
   ';'
```

```diff
SelectedForceAssignment =
+  (Label ':')?
   'with'
   Expression
   'select'
   '?'?
   Target
   '<='
   'force'
   ForceMode?
   SelectedExpressions
   ';'
```

```diff
SelectedWaveformAssignment =
+  (Label ':')?
   'with'
   Expression
   'select'
   '?'?
   Target
   '<='
   DelayMechanism?
   SelectedWaveforms
   ';'
```

## Variable Assignment Statements

```diff
VariableAssignmentStatement =
-  (Label ':')? SimpleVariableAssignment
-| (Label ':')? ConditionalVariableAssignment
-| (Label ':')? SelectedVariableAssignment
+  SimpleVariableAssignment
+| ConditionalVariableAssignment
+| SelectedVariableAssignment
```

```diff
SimpleVariableAssignment =
+  (Label ':')?
   Target
   ':='
   Expression
   ';'
```

```diff
ConditionalVariableAssignment =
+  (Label ':')?
   Target
   ':='
   ConditionalExpressions
   ';'
```

```diff
SelectedVariableAssignment =
+  (Label ':')?
   'with'
   Expression
   'select'
   '?'?
   Target
   ':='
   SelectedExpressions
   ';'
```

## Concurrent Statements

Since a concurrent procedure call without parameters and a component instantiation without a generic or port map are indistinguishable, they are merged into the `ConcurrentProcedureCallOrComponentInstantiationStatement` node:

```diff
+ ConcurrentProcedureCallOrComponentInstantiationStatement
- ConcurrentProcedureCallStatement
```

```diff
ConcurrentStatement =
   BlockStatement
 | ProcessStatement
-| ConcurrentProcedureCallStatement
+| ConcurrentProcedureCallOrComponentInstantiationStatement
 | ConcurrentAssertionStatement
 | ConcurrentSignalAssignmentStatement
 | ComponentInstantiationStatement
 | GenerateStatement
```

```diff
EntityStatement =
   ConcurrentAssertionStatement
-| ConcurrentProcedureCallStatement
+| ConcurrentProcedureCallOrComponentInstantiationStatement
 | ProcessStatement
```

## Selected Items

The LRM writes selected expressions and selected waveforms as a repeated leading part followed by a mandatory trailing one.
For practicality, this was turned into the separated-list form used everywhere else in `vhdl_syntax`: one leading element followed by repeated trailing ones with comma separators.

```diff
SelectedExpressions =
-  (Expression 'when' Choices ',')*
-  Expression
-  'when'
-  Choices
+  SelectedExpressionItem
+  (',' SelectedExpressionItem)*
```

```diff
SelectedWaveforms =
-  (Waveform 'when' Choices ',')*
-  Waveform
-  'when'
-  Choices
+  SelectedWaveformItem
+  (',' SelectedWaveformItem)*
```

The element of a separated list has to be a node reference, so each item is wrapped in a node of its own:

```diff
+ SelectedExpressionItem
+ SelectedWaveformItem
```
