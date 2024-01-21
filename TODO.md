# Unit Tests

## Parser

### Statement:

-   [x] Declaration
-   [x] Return
-   [x] Delete
-   [x] ExpressionStmt
-   [x] Function
-   [x] While
-   [x] For
-   [x] ClassDecl
-   [x] Import
-   [x] Break
-   [x] Continue

### Expression:

-   [x] Method
-   [x] Constructor
-   [x] Range
-   [x] Identifier
-   [x] Assign
-   [x] Prefix
-   [x] Infix
-   [x] If
-   [x] Lambda
-   [x] Call
-   [x] Index
-   [x] Literal
-   [x] Scope

### Literal:

-   [x] Int
-   [x] Float
-   [x] Bool
-   [x] Nil
-   [x] Str
-   [x] Char
-   [x] Array
-   [x] Hash

### Misc:

-   [x] Function Literal With Name

## Evaluator:

### Statement:

-   [x] Declaration
-   [x] Return
-   [x] Delete
-   [x] ExpressionStmt
-   [x] Function
-   [x] While
-   [x] For
-   [ ] ClassDecl
-   [ ] Import
-   [ ] Break
-   [ ] Continue

### Expression:

-   [ ] Method
-   [ ] Constructor
-   [ ] Range
-   [ ] Identifier
-   [ ] Assign
-   [x] Prefix
-   [x] Infix
-   [x] If
-   [x] Lambda
-   [x] Call
-   [x] Index
-   [x] Literal
-   [ ] Scope

### Literal:

-   [x] Int
-   [x] Float
-   [x] Bool
-   [x] Nil
-   [x] Str
-   [x] Char
-   [x] Array
-   [x] Hash

### Misc:

-   [x] Error Handling
-   [x] Closures
-   [x] String Concatenation
-   [x] Builtins
-   [ ] Error Handling with Position Information

## Compiler:

### Statement:

-   [x] Declaration
-   [ ] Return
-   [ ] Delete
-   [x] ExpressionStmt
-   [ ] Function
-   [x] While
-   [ ] For
-   [ ] ClassDecl
-   [ ] Import
-   [ ] Break
-   [ ] Continue

### Expression:

-   [ ] Method
-   [ ] Constructor
-   [ ] Range
-   [ ] Identifier
-   [x] Assign
-   [x] Prefix
-   [x] Infix
-   [x] If
-   [x] Lambda
-   [x] Call
-   [x] Index
-   [x] Literal
-   [ ] Scope

### Literal:

-   [x] Int
-   [x] Float
-   [x] Bool
-   [x] Nil
-   [x] Str
-   [x] Char
-   [x] Array
-   [x] Hash

### Symbol Table:

-   [x] Define Global
-   [x] Resolve Global
-   [x] Define and Resolve Local
-   [x] Resolve Nested Local
-   [x] Define and Resolve Builtins
-   [ ] Define and Resolve Free
-   [ ] Define and Resolve Function

### Misc:

-   [x] Scope
-   [x] Closure
-   [x] Builtins
-   [x] Recursive Function

## VM:

### Statement:

-   [x] Declaration
-   [ ] Return
-   [ ] Delete
-   [x] ExpressionStmt
-   [ ] Function
-   [x] While
-   [x] For
-   [ ] ClassDecl
-   [ ] Import
-   [ ] Break
-   [ ] Continue

### Expression:

-   [ ] Method
-   [ ] Constructor
-   [ ] Range
-   [ ] Identifier
-   [x] Assign
-   [x] Prefix
-   [x] Infix
-   [x] If
-   [ ] Lambda
-   [x] Call
-   [x] Index
-   [ ] Literal
-   [ ] Scope

### Literal:

-   [x] Int
-   [x] Float
-   [x] Bool
-   [x] Nil
-   [x] Str
-   [ ] Char
-   [x] Array
-   [x] Hash

### Misc:

-   [x] Calling Functions with Bindings
-   [x] Error Handling
-   [x] Closures
-   [x] Recursive Functions
-   [x] Builtin

# Opcode:

-   [ ] Implement class method call `Opcode::Method`
-   [ ] Implement `Opcode::Contructor` and `Opcode::Scope`

# Compiler:

-   [ ] Implement `Expression::Assign` for method and index expressions
-   [ ] Implement `ClassObject` method call
-   [ ] Implement `Expression::Scope` and `Expression::Contructor`
