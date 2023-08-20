package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parse_state_xmacro",
    textual_hdrs = ["parse_state.xmacro.h"],
)

cc_library(
    name = "parse_tree",
    hdrs = ["parse_tree.h"],
    deps = [
        "//toolchain/lexer:token",
    ],
)

cc_library(
    name = "parser",
    hdrs = ["parser.h"],
    srcs = ["parser.cc"],
    deps = [
        ":parse_state_xmacro",
        ":parse_tree",
        "//toolchain/lexer:token",
        "//toolchain/lexer:token_buffer",
        "@asoffer_nth//nth/debug",
    ],
)

cc_test(
    name = "parser_test",
    srcs = ["parser_test.cc"],
    deps = [
        "//toolchain/lexer",
        "//toolchain/lexer:token_matchers",
        ":parser",
        "@asoffer_nth//nth/test:main",
    ],
)
