# Navigating the codebase

## Style

_Below are the rules and guidelines dictating the style in which code is written in the Icarus codebase. The reasons for these rules vary, from performance to readability to consistency with existing code. We believe that reasonable people can disagree on nearly all of the choices here, but that differing opinion is not reason enough to deviate from these guidelines._

We use C++20 and a style that is largely based on the [Google Style Guide](https://google.github.io/styleguide/). In particular, we do not use exceptions. We also make the following additions and changes:

* Enum class enumerators should be `PascalCase` and do not need to be prefixed with a `k` as in `kConstantCase`.
* Do not use `&&`, `||`, or `!`. Use the keyword versions `and`, `or`, and `not` respectively.
* Do not use `std::printf` or similar functions. Prefer `absl::Printf` and variants defined in `@com_google_absl//absl/strings:str_format`.
* Do not use `std::shared_ptr`. The shared-pointer abstraction should be entirely avoided.
* Do not use `std::mutex`. Prefer `absl::Mutex`.
* All command-line flags must be defined using `ABSL_FLAG` in the same file defining `main()` for the binary.
* Avoid RTTI except for debugging facilities. In other words, optimized builds should have no use of RTTI.

### Formatting

All code formatting should be handled by clang-format using the checked in [.clang-format](https://github.com/asoffer/Icarus/blob/main/.clang-format) options file. There are very few instances where we turn off clang-formatting in the source. Use your judgment, but lean heavily towards preferring the automatically formatted code. The only notable exception where we expect to commonly turn off clang-format is with C++20 concepts (which are not yet supported by clang-format).

## High-level design philosophy

The Icarus project intends to build high-quality libraries and executables for working with the Icarus programming language. This includes an interpreter, compiler, REPL, code formatter, syntax-tree matching library, and several other language extension mechanisms. We wish for the codebase to be strongly modular. Each binary should only require the components it needs. For example, the code formatter may rely on the syntax tree, but should not require any type-checking facilities.

## Layout of source code

This section describes the design of the Icarus codebase, both in its current state and the intended long-term state.

### Current directory structure

* `//base` -- Common utilities that might be useful in any project, whether or not that project is related to programming language infrastructure. `//base` depends on nothing outside of its package.
* `//core` -- Common utilities that are specific to programming language infrastructure. This includes things like strong-types for type sizes and alignment, function parameters and arguments, etc. `//core` may only depend on itself and `//base`.
* `//ast` -- Holds the entirety of the abstract syntaxt tree.
* `//ir/value` -- Holds everything needed for values used in the intermediate representation.
* `//frontend` -- All lexing and parsing.
* `//semantic_analysis` -- All type-checking and bytecode generation is done in this package. There are two subpackage, `//semantic_analysis/type_verification` and `//semantic_analysis/byte_code` which handle type-checking and bytecode generation respectively. Each package has roughly one target for each AST node type.
* `//bazel` -- Specification of the C++ Toolchain used to compile this project.

### Expected directory structure changes

#### Frontend changes

Parsing and lexing should be made more general and specified by the compiler binary rather than hard-coded. Even if the infrastructure is only ever used for Icarus, this is beneficial because it allows us to inject annotations for the purposes of testing or add special nodes so that AST matchers can be specified in a language similar to the source (rather than a DSL as is done with `clang::ast_matchers`).

Likely the simplest way to do this is to have each AST node come with a description of how it should be parsed and have the parser dependent on a list of AST nodes it knows how to generate. This work has not been started at all and probably requires significant thought and design before doing so.
