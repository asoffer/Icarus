package(default_visibility = ["//compiler:__subpackages__"])

cc_library(
    name = "access",
    srcs = ["access.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//diagnostic:errors",
        "//type:enum",
        "//type:flags",
        "//type:overload_set",
        "//type:primitive",
        "//type:qual_type",
        "//type:struct",
    ],
)

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
        "//diagnostic:errors",
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
        "//diagnostic:errors",
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
        "//ast",
        "//compiler:compiler_header",
        "//compiler/verify/internal:qual_type_iterator",
        "//diagnostic:errors",
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
        "//diagnostic:errors",
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
        "//diagnostic:errors",
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
    name = "common",
    srcs = ["common.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler/dispatch:match",
        "//compiler/dispatch:parameters_and_arguments",
        "//core:fn_args",
        "//diagnostic:errors",
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
        "//diagnostic:errors",
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
    name = "identifier",
    srcs = ["identifier.cc"],
    deps = [
        "//ast",
        "//base:defer",
        "//compiler:compiler_header",
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
    name = "index",
    srcs = ["index.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//diagnostic:errors",
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
    name = "struct_literal",
    srcs = ["struct_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//diagnostic:errors",
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
