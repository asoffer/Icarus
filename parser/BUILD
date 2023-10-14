package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parse_tree_node_kind_xmacro",
    textual_hdrs = ["parse_tree_node_kind.xmacro.h"],
)

cc_library(
    name = "parse_state_xmacro",
    textual_hdrs = ["parse_state.xmacro.h"],
)

cc_library(
    name = "parse_tree",
    hdrs = ["parse_tree.h"],
    srcs = ["parse_tree.cc"],
    deps = [
        ":parse_tree_node_kind_xmacro",
        "//lexer:token",
        "@asoffer_nth//nth/container:interval",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/strings:interpolate",
        "@asoffer_nth//nth/utility:iterator_range",
    ],
)

cc_library(
    name = "parser",
    hdrs = ["parser.h"],
    srcs = ["parser.cc"],
    deps = [
        ":parse_state_xmacro",
        ":parse_tree",
        ":precedence",
        "//common/language:primitive_types",
        "//diagnostics/consumer",
        "//lexer:token",
        "//lexer:token_buffer",
        "@asoffer_nth//nth/debug",
    ],
)

cc_test(
    name = "parser_test",
    srcs = ["parser_test.cc"],
    deps = [
        ":parser",
        "//common:resources",
        "//diagnostics/consumer:null",
        "//lexer",
        "//lexer:token_matchers",
        "@asoffer_nth//nth/test:main",
    ],
)

cc_library(
    name = "precedence_groups_xmacro",
    textual_hdrs = ["precedence_groups.xmacro.h"],
)

cc_library(
    name = "precedence",
    hdrs = ["precedence.h"],
    srcs = ["precedence.cc"],
    deps = [
        ":precedence_groups_xmacro",
        "@asoffer_nth//nth/debug",
    ],
)
