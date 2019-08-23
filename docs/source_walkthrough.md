# C2Rust Source Walkthrough

This guide provides insight into the program structure of the c2rust translator and should be helpful to anyone wanting contribute to its development.

This project provides tooling for translating C programs into Rust, refactoring Rust programs, and cross-checking the execution of C and Rust programs.

## Project crate structure

The c2rust project is divided into 6 different crates. The purposes of each crate is described below.

### c2rust

The c2rust crate provides a unified command-line interface to the translator and to the refactorer. This is intended to the be the top-level crate that a user would install and interact with.

This crate contains logic for dispatching command-line arguments to the correct sub crate. It should not contain any logic for translating or refactoring code itself.

### c2rust-ast-builder

The c2rust-ast-builder crate provides an AST building abstraction on top of rustc's libsyntax. This is used for code generation both in translation and refactoring.

The builder implemented in this package provides a more stable interface to AST generation that we'd get depending directly on libsyntax. Libsyntax itself is consided unstable and subject to change dramatically at each nightly release. Libsyntax provides is own AST building functionality, but it doesn't have many of the conveniences that we use in our own implementation.

### c2rust-ast-exporter

The c2rust project uses clang as a library in order to get reliable pre-processing, parsing, and type-checking of C code. The c2rust-ast-exporter crate provides a mix of C++ and Rust in order to provide a dump of the clang-generated AST. The exporter exports the AST using [CBOR](http://cbor.io).

### c2rust-transpile

This crate implements all of the translation logic for getting from C to Rust. It consumes input from the c2rust-ast-exporter crate and generates Rust using c2rust-ast-builder. It is invoked by the c2rust crate.

### c2rust-refactor

This crate implements various rewrites and analyses for refactoring our generated Rust code into more idiomatic code.

### cross-checks

This crate provides tools for instrumenting Rust executables to be suitable for running in the multi-variant execution engine.

## Crate Walkthrough: c2rust-ast-builder

### Builder type

The `Builder` type allows for short-cuts when building up AST elements by providing a place to store default values for attributes that typically are not changed. This effectively allows us to simulate optional arguments to methods.

New `Builder` values can be constructed using `mk()`.

For example the default behavior is for patterns to be immutable. If we want to emit a mutable pattern we can store the mutability flag on the `Builder` and then generate a pattern.

```rust
let mut_x_pat = mk().mutbl().ident_pat("x"); // generates: mut x
let y_pat = mk().ident_pat("y"); // generates: y
```

### Make trait

The `Make` trait allows for convenient, implicit coercions when using the `Builder`. Many methods will be parameterized over an arbitrary `Make` implementation to avoid needing manual conversions. It's quite common to see methods requiring `Make<Ident>` arguments instead of `Ident` so that we can accept a number of types. Any new methods implemented for `Builder` should look to see if there are useful `Make` implementations for its argument types.

```rust
pub trait Make<T> { fn make(self, mk: &Builder) -> T; }
```

### P type

The `P` type comes from the `libsyntax` crate and provides functionality similar to `Box` for immutable, shared values. Many components of the Rust AST will store `P<T>` instead of `T` when there are potential savings to be had from shared references.

### Spans and Node IDs

The Rust AST types are designed to be able to be cross-referenced to source-file locations and various type-information metadata maps. These references are tracked through span and node IDs scattered throughout the AST type definitions. In the case of generating new syntax we don't have any corresponding metadata maps to align with. Instead we fill all of these ID fields with various dummy values: `DUMMY_SP` and `DUMMY_NODE_ID`.

### Naming convention

Builder methods are named using the pattern `kind_type`. For example to make a `P<Ty>` that is a pointer to another `Ty` use the `ptr_ty` method because internally you're making a `TyKind::Ptr`.

## Crate Walkthrough: c2rust-transpile

The `c2rust-transpile` crate is broken up into 4 major pieces. First the `c_ast` modules are used to import and handle the C AST representation of the program to be translated. The `cfg` modules implement Relooper logic to compile away control-flow constructs that exist in C but not in Rust. In particular we use this to get rid of `goto` and `switch` statements. The `translator` modules do the bulk of the actual translation for declarations, statements, and expressions. The `rust_ast` modules provide helpers for generating the final Rust code.

In order to preserve comments across translations we instruct clang to parse and export all comments. The mechanism provided by `libsyntax` for emitting comments relies on matching up comments with particular _span IDs_, so we have to carefully track the span IDs of the Rust AST that we generate when we want to associate a comment. Additionally `libsyntax` requires that span IDs are found in order, so before we emit the final code we renumber all span IDs to be in order.

### Module structure

The `c_ast` module provides the Rust types that mirror the AST from Clang along with methods for deserializing from CBOR into these types. There are 4 kinds of AST element that we distinguish between: Types, Expressions, Declarations, and Statements. Each of these has a corresponding enum: `CTypeKind`, `CExprKind`, `CDeclKind`, and `CStmtKind`. All of these IDs can be dereferenced using the indexing operator on a `TypedAstContext`. The _typed_ part of that name is to distinguish it from the conversion time `ConversionContext` where raw CBOR nodes are processed before we know which IDs correspond to one of the 4 categories above.

The `c_ast.iterator` module provides a depth-first iterator of the four C AST types. This is used in the code to query different kinds of properties of the input code when translating. For example it can be used to check if a section of code uses a goto statement.

The `rust_ast` module provides functionality for working with the Rust AST and not for building it. Build functionality can be found in the `c2rust-ast-builder` crate.

The `rust_ast.traverse` module provides a depth-first visitor pattern for Rust ASTs. Transformations can be written and made to be instances of the `Traversal` trait to specify the desired behavior at each AST element. The default implementations of the trait's methods will simply recursively apply the traversal to the child nodes. This can be used both to transform ASTs as well as to query them.

The `rust.comment_store` module handles the logic for tracking comments that are going to be reinserted back into the final generated Rust.

### Translation type

This is a substantial type that carries the running state of a translation. This struct carries keeps track of all of the items generated so far, the language features used, the association between declarations and their Rust identifiers, and the configuration values set for this translation.

There are various methods for translating elements of C syntax defined on this struct. When these are used any supporting items, imports, or features will be tracked in addition to returning the translated item as the method result.

### WithStmts type

The `WithStmts` type is a convenient way to keep track of all of the supporting statements that go along with a value after it has been translated. When translating an expression that will not be used for anything other than its side effects, the `val` component will be set to a panic macro to make it easy to detect the mistake in the generated code.

### ExprContext type

The `ExprContext` struct tracks information about how to translate expressions. This value is updated as translation progresses through the AST keeping track of the different contexts that expression translation can be in.

The `used` attribute has one of the biggest effects on translation. This indicates when the `val` field of the `WithStmts` result is going to be emitted or discarded. This can be modified in the current context with the `.used()` and `.unused()` methods.

The `is_static` attribute indicates that an expression is being used in the initializer for a static variable. Rust has many extra restrictions on the expressions that can be used for a static initializer. In some cases we can still generate valid code at the cost of readability. This fallback is enabled by this attribute.

The `decay_ref` attribute keeps track of whether or not we're in a context in which Rust will infer that a reference can decay in to a pointer. This can happen at method calls, variable initializers, and possibly more locations. This allows the translation to omit some otherwise superfluous casts.

The `va_decl` attribute indicates which, if any, declaration corresponds to the variable-argument list for the current variadic function. This enables us to drop the associated declaration, `va_start`, and `va_end` for that variable during translation.

### Handling Comments

Comments are tricky to translate. Part of the issue is that comments are typically removed from C source code as part of the pre-processing phase. To handle this we extract comments from the original source code and track their source position information. In addition we track source position information for the various syntactic elements of the C translation unit.

During translation of a statement or a declaration we look into the set of comments to see if we're as close as we're going to get to the home location of this comment. If so, we are allocated a unique, temporary span ID to associate between the comment and the syntax element that should carry the comment. 

Once all of the translation is complete we revisit all of the synthetic span IDs to reassign them so that they are in ascending order, as required by `libsyntax`. Once we've done this renumbering, `libsyntax` is able to emit the comments in the correct location during the final rendering of the Rust AST.

### Named References

The `translator.named_references` provides support for naming expressions that need to be able to be read or written two multiple times without reevaluating the expression. This module helps by identifying when a temporary variable will be needed to hold onto a reference so that it can support read and write operations.
