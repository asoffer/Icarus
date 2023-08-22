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
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "parser",
    hdrs = ["parser.h"],
    srcs = ["parser.cc"],
    deps = [
        ":parse_state_xmacro",
        ":parse_tree",
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
        "//diagnostics/consumer:null",
        "//lexer",
        "//lexer:token_matchers",
        "@asoffer_nth//nth/test:main",
    ],
)
