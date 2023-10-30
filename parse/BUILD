package(default_visibility = ["//visibility:public"])

cc_library(
    name = "node",
    hdrs = ["node.h"],
    srcs = ["node.cc"],
    deps = [
        ":node_xmacro",
        "//common:strong_index_type",
        "//lexer:token",
        "@asoffer_nth//nth/strings:interpolate",
    ],
)

cc_library(
    name = "node_xmacro",
    textual_hdrs = ["node.xmacro.h"],
)

cc_library(
    name = "parser",
    hdrs = ["parser.h"],
    srcs = ["parser.cc"],
    textual_hdrs = ["state.xmacro.h"],
    deps = [
        ":precedence",
        ":tree",
        "//common:debug",
        "//common/language:atoms",
        "//diagnostics/consumer",
        "//ir:scope",
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
    name = "precedence",
    hdrs = ["precedence.h"],
    srcs = ["precedence.cc"],
    deps = [
        "//common/language:precedence",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "tree",
    hdrs = ["tree.h"],
    srcs = ["tree.cc"],
    deps = [
        ":node",
        "@asoffer_nth//nth/container:interval",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/utility:iterator_range",
    ],
)
