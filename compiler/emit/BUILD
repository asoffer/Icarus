package(default_visibility = ["//compiler:__subpackages__"])

cc_library(
    name = "access",
    srcs = ["access.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler:library_module",
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
    name = "binary_operator",
    srcs = ["binary_operator.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "binary_operator_test",
    srcs = ["binary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "cast",
    srcs = ["cast.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "cast_test",
    srcs = ["cast_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "goto",
    srcs = ["goto.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

# Note: There are no tests for ":goto" directly as these are well-covered by
# ":scope_node_test".

cc_library(
    name = "identifier",
    srcs = ["identifier.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

# cc_test(
#     name = "identifier_test",
#     srcs = ["identifier_test.cc"],
#     deps = [
#         "//compiler",
#         "//test:module",
#         "@com_google_googletest//:gtest_main",
#     ],
# )

cc_library(
    name = "index",
    srcs = ["index.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
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
    name = "miscellaneous",
    srcs = ["miscellaneous.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//ir/value",
    ],
)

# TODO: Miscellaneous test

cc_library(
    name = "scope_node",
    srcs = ["scope_node.cc"],
    deps = [
        "//ast",
        "//base:defer",
        "//compiler:compiler_header",
        "//compiler/dispatch:parameters_and_arguments",
        "//ir:compiled_scope",
        "//ir/instruction:inliner",
        "//ir/value:scope",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_test(
    name = "scope_node_test",
    srcs = ["scope_node_test.cc"],
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
    ],
)

cc_test(
    name = "terminal_test",
    srcs = ["terminal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "unary_operator",
    srcs = ["unary_operator.cc"],
    deps = [
        "//ast",
        "//frontend/lex:operators",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "unary_operator_test",
    srcs = ["unary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)
