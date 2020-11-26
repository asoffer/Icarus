# Navigating the codebase

## Style

_Below are the rules and guidelines dictacting the style in which code is written in the Icarus codebase. The reasons for these rules vary from performance to readability to consistency with already existing code. We believe that reasonable people can disagree on nearly all of the choices here, but that differing opinions is not reason enough to deviate from these guidelines._

We use C++20 and a style that is largely based on the [Google Style Guide](https://google.github.io/styleguide/). In particular, we do not use exceptions. We also make the following additions and changes:

* Enum class enumerators should be `PascalCase` and do not need to be prefixed with a `k` as in `kConstantCase`.
* Do not use `&&`, `||`, or `!`. Use the keyword versions `and`, `or` and `not` repsepectively.
* We do not use `std::printf` or similar functions. Prefer `absl::Printf` and variants defined in `@com_google_absl//absl/strings:str_format`.
* Do not use `std::shared_ptr`. The shared-pointer abstraction should be entirely avoided.
* Do not use `std::mutex`. Prefer `absl::Mutex`.
* Do not use C++20 coroutines.
* All command-line flags must be defined using `ABSL_FLAG` in the same file defining `main()` for the binary.
* Avoid RTTI except for debugging facilities. In other words, optimized builds should have no use of RTTI.

### Formatting

All code formatting should be handled with clang-format and the checked in [.clang-format](https://github.com/asoffer/Icarus/blob/main/.clang-format) options file. There are very few instances where we turn off clang-formatting in source. Use your judgment but lean heavily towards preferring the automatically formatted code. The only notable exception where we expect to commonly turn off clang-format is with C++20 concepts which are not yet supported by clang-format.

### External dependencies

Icarus only depends on the C++ standard library, Abseil, LLVM, and GoogleTest.

## High-level design philosophy

The Icarus project intends to build high-quality libraries and executables for working with the Icarus programming language. This includes an interpretter, compiler, REPL, code formatter, syntax-tree matching library, and several other language extension mechanisms. We wish for the codebase to be strongly modular. Each binary should only require the components they require. For example, the code formatter may rely the syntax tree, but should not require any type-checking facilities

## Layout of source code

This section describes the design of the Icarus codebase, in its current state, and its intended long-term state.

### Current directory structure
* `//base` -- Common utilities that might be useful in any project, whether or not that project is related to programming language infrastructure. `//base` depends on nothing outside of its package.
* `//core` -- Common utilities that are specific to programming language infrastructure. This includes things like strong-types for type sizes and alignment, function parameters and arguments, etc. `//core` may only depend on itself and `//base`.
* `//type` -- Holds everything corresponding to the Icarus type system.
* `//ast` -- Holds the entirety of the abstract syntaxt tree.
* `//ir` -- Holds everything needed for the intermediate representation (values in `//ir/value`, instructions in `//ir/instruction`, basic blocks and functions in `//ir/blocks`, and an interpretter in `//ir/interpretter`). Ideally, nothing in `//ir` would depend on anything outside `//base`, `//core`, other than `type::Type`.
* `//frontend` -- All lexing and parsing.

### Expected directory structure changes

#### Interpretter changes
As a long-term goal, we want to make the IR interpretter highly extensible, by having the interpretter templated on the set of instructions available. The interpretter will come with several core instructions (phi-nodes, loads, stores, function calls, and jumps). All other instructions will depend on the interpretter infrastructure and must themselves describe how to be executed. To achieve this goal we intend to move most instructions that out of `//ir/instruction` and closer to their use. For example, `ArrayInstruction` which constructs an array type from a type and a length has been moved already to `//type:array`, adjacent to the `type::Array` struct.

There are several benefits to this extensibility, even if the interpretter is only ever used for the Icarus language.
* Long-term changes to the language are made easier.
* We can add mock/fake instructions to be used during testing to inspect the IR.
* We can more easily add compile-time-only instructions (for example, baking in a debugger to the interpretter that executes code at compile-time.

#### Type system changes
One oddity you may have noticed in the layering described above is that `//ir` is allowed to depend on `//base`, `//core`, and also one particular target in the `//type` package (the one defining the struct `type::Type`. We would like this rule to be clearer by removing this one exception. To do this, we will move `type::Type` into `//core`. This makes sense because `type::Type` (to be renamed to `core::Type`) is a type-erased container for any type used in the language. Instructions and IR values in general need to know about types but not about any specific kind of types so the type-erased wrapper makes sense here. This migration has been started but is far from finished. We used to have all types represented by a pointer (now named `type::LegacyType*`). One of the changes that needs to be made is to remove the inheritance hierarchy of types, replacing them simply with a type-erasure mechanism, none of which has been started yet.

#### Compilation changes
Currently `//compiler/verify` has one build target for each AST node's type verification code. We wish to do the same thing for code generation in `//compiler/emit`. There are still some parts of this migration that are not yet complete. In particular, some code generating functions are still in `//compiler/emit_value.cc` and `//compiler/emit_function_call_infrastructure.cc`.

#### Frontend changes
Parsing and lexing should be made more general and specified by the compiler binary rather than hard-coded. Even if the infrastructure is only ever used for Icarus, this is beneficial because it allows us to inject annotations for the purposes of testing or add special nodes so that AST matchers can be specified in a language similar to the source (rather than a DSL as is done with `clang::ast_matchers`).

Likely the simplest way to do this is to have each AST node come with a description of how it should be parsed and have the parser dependent on a list of AST nodes it knows how to generate. This work has not been started at all and probably requires significant thought and design before doing so.
