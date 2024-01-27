package(default_visibility = ["//visibility:public"])

cc_library(
    name = "declaration",
    hdrs = ["declaration.h"],
    deps = [
        ":node_index",
    ],
)

cc_library(
    name = "node",
    hdrs = ["node.h"],
    deps = [
        ":declaration",
        ":node_index",
        ":node_xmacro",
        "//ir:lexical_scope",
        "//lexer:token",
        "@nth_cc//nth/strings:interpolate",
    ],
)

cc_library(
    name = "node_index",
    hdrs = ["node_index.h"],
    srcs = ["node_index.cc"],
    deps = [
        "//common:strong_index_type",
    ],
)

cc_library(
    name = "node_xmacro",
    textual_hdrs = ["node.xmacro.h"],
)

cc_library(
    name = "state_xmacro",
    textual_hdrs = ["state.xmacro.h"],
    deps = [":node_xmacro"],
)

cc_library(
    name = "parser",
    hdrs = ["parser.h"],
    srcs = ["parser.cc"],
    deps = [
        ":declaration",
        ":node_index",
        ":node_xmacro",
        ":precedence",
        ":state_xmacro",
        ":tree",
        "//common:debug",
        "//common/language:atoms",
        "//diagnostics/consumer",
        "//ir:lexical_scope",
        "//lexer:token",
        "//lexer:token_buffer",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "precedence",
    hdrs = ["precedence.h"],
    srcs = ["precedence.cc"],
    deps = [
        "//common/language:precedence",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "tree",
    hdrs = ["tree.h"],
    srcs = ["tree.cc"],
    deps = [
        ":node",
        ":node_index",
        "@nth_cc//nth/container:interval",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/utility:iterator_range",
    ],
)
