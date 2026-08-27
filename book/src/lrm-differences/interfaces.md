# Interfaces

For practicality, the interface object declarations were merged into a single production, with an `InterfaceObjectClass` to tell them apart:

```diff
- InterfaceConstantDeclaration
- InterfaceSignalDeclaration
- InterfaceVariableDeclaration
+ InterfaceObjectClass
```

```diff
InterfaceObjectDeclaration =
-  InterfaceConstantDeclaration
-| InterfaceSignalDeclaration
-| InterfaceVariableDeclaration
-| InterfaceFileDeclaration
+  InterfaceObjectClass?
+  IdentifierList
+  ':'
+  Mode?
+  SubtypeIndication
+  'bus'?
+  (':=' Expression)?
```

A file cannot be merged in the same way, so the `InterfaceFileDeclaration` was lifted out and became an alternative of the `InterfaceDeclaration`:

```diff
InterfaceDeclaration =
   InterfaceObjectDeclaration
+| InterfaceFileDeclaration
 | InterfaceTypeDeclaration
 | InterfaceSubprogramDeclaration
 | InterfacePackageDeclaration
```

Similarly, the three forms of the `InterfacePackageGenericMapAspect` were merged into one, with an `InterfacePackageGenericMapAspectInner` node to differentiate them:

```diff
InterfacePackageGenericMapAspect =
-  GenericMapAspect
-| 'generic' 'map' '(' '<>' ')'
-| 'generic' 'map' '(' 'default' ')'
+  'generic'
+  'map'
+  '('
+  InterfacePackageGenericMapAspectInner
+  ')'
```

```diff
+ InterfacePackageGenericMapAspectInner
```
