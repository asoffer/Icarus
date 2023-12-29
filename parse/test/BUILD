package(default_visibility = ["//visibility:public"])

cc_library(
    name = "matchers",
    hdrs = ["matchers.h"],
    testonly = True,
    deps = [
        ":tree_node_ref",
        "//parse:node_xmacro",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/test",
    ],
)

cc_library(
    name = "tree_node_ref",
    hdrs = ["tree_node_ref.h"],
    testonly = True,
    deps = [
        "//parse:node",
        "//parse:node_index",
        "//parse:tree",
        "@asoffer_nth//nth/base:attributes",
        "@asoffer_nth//nth/container:stack",
        "@asoffer_nth//nth/meta:type",
    ],
)

COMMON_PARSER_TEST_DEPS = [
    ":matchers",
    ":tree_node_ref",
    "//diagnostics/consumer:null",
    "//lexer",
    "//lexer:token_matchers",
    "//parse:parser",
    "@asoffer_nth//nth/test:main",
]

cc_test(
    name = "access",
    srcs = ["access.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "assignment",
    srcs = ["assignment.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "basic",
    srcs = ["basic.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "enum",
    srcs = ["enum.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "declaration",
    srcs = ["declaration.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "function_literal",
    srcs = ["function_literal.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "if_statement",
    srcs = ["if_statement.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "index",
    srcs = ["index.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "invoke",
    srcs = ["invoke.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "interface",
    srcs = ["interface.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "operator_precedence",
    srcs = ["operator_precedence.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "scope",
    srcs = ["scope.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "unary_operator",
    srcs = ["unary_operator.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)

cc_test(
    name = "while",
    srcs = ["while.cc"],
    deps = COMMON_PARSER_TEST_DEPS,
)
