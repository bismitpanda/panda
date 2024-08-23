# Panda

This is a toy interpreter for the `panda` programming language. It has a AST Tree walker based evaluater as well as a Compiler + VM structure. It is superficially based on the `monkey` programming language.

## Example

```
var hello = "Hello,";
var world = "World!";

print(hello, world); // Output: Hello, World!
```

## Features

-   Mutable and Immutable variables. Immutable variables use `const` keyword.
-   Added Assignment Expressions. `IDENTIFIER`'s, `INDEX_EXPRESSION`'s and `METHOD_EXPRESSIONS`'s can be assigned to
-   Added `CHAR` data type based on rust's `char`
-   Added `while` and `for ` loops.
-   Added a few new builtin functions.
-   Added the `nil` keyword and its equality comparison with others
-   Added function statement in `fn <IDENT>(<ARGUMENTS>) <BODY>` format.
-   Added `FLOAT` data-type based on rust's `f64`
-   Added bitwise operators `>>`, `<<`, `&`, `|`, `^` and boolean operators `&&` and `||` (These two are valid for all data types and are [short-circuit evaluated](http://en.wikipedia.org/wiki/Short-circuit_evaluation)).
-   `STR`'s are a `Vec<char>` instead of standard `String`/`&str` as it makes manipulation easier.
-   Changed `is_truthy()` evaluation criteria and makin it similar to `python`'s implementation with empty strings, arrays and hashes as well zero (both for `INT` and `FLOAT`) being _`falsey`_.
-   Restricted allowed types inside arrays.
-   Added rust style range expression using `..` with reverse, negative and step support in the format `<EXPR>..<EXPR>[..<EXPR>]`.
-   Added `STR` and `ARRAY` slicing using `RANGE`.
-   Added a simple type system.
-   Added `import` statements and scope expressions using `::` for accessing module members.
-   Added `STR` and `CHAR` character escaping.
-   Added nil variable declaration _i.e._ declaring variable without any value.
-   Added `delete` statement to remove variables

## Todo

-   List all the `features` in the above section.
-   Finalize a formal grammar for future reference.
-   Create a standard library for use in import systems.
-   Add a type system with more features like runtime type checking (_i.e._ wrapping rust `Objects` inside panda's `std` types).
-   Write tests for every `AST` and `Object`.
