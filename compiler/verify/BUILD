package(default_visibility = ["//compiler:__subpackages__"])

cc_library(
    name = "access",
    srcs = ["access.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler:library_module",
        "//ir/value:module_id",
        "//type:enum",
        "//type:flags",
        "//type:overload_set",
        "//type:primitive",
        "//type:qual_type",
        "//type:struct",
    ],
)

cc_library(
    name = "block_literal",
    srcs = ["block_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:primitive",
        "//type:qual_type",
    ],
)

# TODO: block_literal_test

cc_test(
    name = "access_test",
    srcs = ["access_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "array_literal",
    srcs = ["array_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:array",
        "//type:primitive",
        "//type:qual_type",
    ],
)

cc_test(
    name = "array_literal_test",
    srcs = ["array_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "array_type",
    srcs = ["array_type.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:array",
        "//type:primitive",
        "//type:qual_type",
    ],
)

cc_test(
    name = "array_type_test",
    srcs = ["array_type_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "assignment",
    srcs = ["assignment.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//compiler/verify/internal:qual_type_iterator",
        "//type:primitive",
        "//type:qual_type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "assignment_test",
    srcs = ["assignment_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "binary_operator",
    srcs = ["binary_operator.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:pointer",
        "//type:primitive",
        "//type:qual_type",
    ],
)

cc_test(
    name = "binary_operator_test",
    srcs = ["binary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "//type:primitive",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "call",
    srcs = ["call.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:callable",
        "//type:generic_struct",
        "//type:qual_type",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_test(
    name = "call_test",
    srcs = ["call_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "cast",
    srcs = ["cast.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//type:qual_type",
    ],
)

cc_test(
    name = "cast_test",
    srcs = ["cast_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "common",
    hdrs = ["common.h"],
    srcs = ["common.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler:context",
        "//compiler:library_module",
        "//core:dependency_node",
        "//core:arguments",
        "//ir/value",
        "//type:callable",
        "//type:overload_set",
        "//type:typed_value",
    ],
)
# TODO: Tests for things in common. These are largely covered by other AST-node 
# tests, but we should cover them explicitly.

cc_library(
    name = "comparison_operator",
    srcs = ["comparison_operator.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:array",
        "//type:pointer",
        "//type:primitive",
        "//type:qual_type",
        "//type:struct",
        "//type:tuple",
    ],
)

cc_test(
    name = "comparison_operator_test",
    srcs = ["comparison_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "declaration",
    srcs = ["declaration.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler:library_module",
        "//compiler/verify/internal:assignment_and_initialization",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "declaration_test",
    srcs = ["declaration_test.cc"],
    deps = [
        "//compiler",
        "//test:module",

        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "designated_initializer",
    srcs = ["designated_initializer.cc"],
    deps = [
        "//ast",
        "//base:defer",
        "//compiler:compiler_header",
        "//compiler/verify/internal:qual_type_iterator",
        "//type:primitive",
        "//type:qual_type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "designated_initializer_test",
    srcs = ["designated_initializer_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "enum_literal",
    srcs = ["enum_literal.cc"],
    deps = [
        "//ast",
        "//base:defer",
        "//compiler:compiler_header",
        "//type:primitive",
        "//type:qual_type",
    ],
)

cc_test(
    name = "enum_literal_test",
    srcs = ["enum_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "function_literal",
    srcs = ["function_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler:extract_jumps",
        "//compiler/verify:common",
        "//type:cast",
        "//type:qual_type",
    ],
)

cc_test(
    name = "function_literal_test",
    srcs = ["function_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "function_type",
    srcs = ["function_type.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "function_type_test",
    srcs = ["function_type_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "goto",
    srcs = ["goto.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:primitive",
    ],
)

cc_test(
    name = "goto_test",
    srcs = ["goto_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "identifier",
    srcs = ["identifier.cc"],
    deps = [
        "//ast",
        "//base:defer",
        "//compiler:compiler_header",
        "//compiler:cyclic_dependency_tracker",
        "//type:overload_set",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "identifier_test",
    srcs = ["identifier_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "import",
    srcs = ["import.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler:library_module",
        "//ir/value:module_id",
    ],
)

cc_test(
    name = "import_test",
    srcs = ["import_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "index",
    srcs = ["index.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:array",
        "//type:primitive",
        "//type:qual_type",
    ],
)

cc_test(
    name = "index_test",
    srcs = ["index_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "jump",
    srcs = ["jump.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:pointer",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "jump_test",
    srcs = ["jump_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "miscellaneous",
    srcs = ["miscellaneous.cc"],
    deps = [
        "//ast",
        "//base:defer",
        "//compiler:compiler_header",
        "//type:primitive",
        "//type:qual_type",
    ],
)

# TODO: miscellaneous_test


cc_library(
    name = "parameterized_struct_literal",
    srcs = ["parameterized_struct_literal.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:library_module",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

# TODO: parameterized_struct_literal

cc_library(
    name = "scope_literal",
    srcs = ["scope_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:jump",
        "//type:primitive",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "scope_literal_test",
    srcs = ["scope_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "//type:primitive",
        "//type:jump",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "short_function_literal",
    srcs = ["short_function_literal.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//type:qual_type",
    ],
)

cc_test(
    name = "short_function_literal_test",
    srcs = ["short_function_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "struct_literal",
    srcs = ["struct_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "struct_literal_test",
    srcs = ["struct_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "terminal",
    srcs = ["terminal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:primitive",
        "//type:qual_type",
    ],
)

cc_test(
    name = "terminal_test",
    srcs = ["terminal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "//type:primitive",
        "//type:qual_type",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "unary_operator",
    srcs = ["unary_operator.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//compiler/verify/internal:qual_type_iterator",
        "//type:pointer",
        "//type:primitive",
        "//type:qual_type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "unary_operator_test",
    srcs = ["unary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "//type:array",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ],
)
