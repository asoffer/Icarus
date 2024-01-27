package(default_visibility = ["//visibility:public"])

cc_library(
    name = "lex",
    hdrs = ["lex.h"],
    srcs = ["lex.cc"],
    deps = [
        ":lex_impl",
    ],
)

cc_library(
    name = "lex_impl",
    hdrs = ["lex_impl.h"],
    visibility = ["//visibility:private"],
    deps = [],
)

cc_library(
    name = "lexer",
    hdrs = ["lexer.h"],
    srcs = ["lexer.cc"],
    deps = [
        ":lex",
        ":lex_impl",
        ":token",
        ":token_buffer",
        "//diagnostics/consumer",
        "@nth_cc//nth/debug",
    ],
)

cc_test(
    name = "lexer_test",
    srcs = ["lexer_test.cc"],
    deps = [
        ":lexer",
        ":token_matchers",
        "//diagnostics/consumer:null",
        "@nth_cc//nth/test:main",
    ],
)

cc_library(
    name = "token_kind_xmacro",
    textual_hdrs = ["token_kind.xmacro.h"],
    deps = [
        "//common/language:primitive_types",
    ],
)

cc_library(
    name = "token_matchers",
    testonly = True,
    hdrs = ["token_matchers.h"],
    deps = [
        ":token",
        "//common:identifier",
        "@nth_cc//nth/debug/property",
    ],
)

cc_library(
    name = "token",
    hdrs = ["token.h"],
    srcs = ["token.cc"],
    deps = [
        ":token_kind_xmacro",
        "//common:identifier",
        "//common:integer",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/io:printer",
        "@nth_cc//nth/strings:interpolate",
        "@nth_cc//nth/strings/format:universal",
    ],
)

cc_test(
    name = "token_test",
    srcs = ["token_test.cc"],
    deps = [
        ":token",
        "@nth_cc//nth/test:main",
        "@nth_cc//nth/io:string_printer",
        "@nth_cc//nth/strings:interpolate",
        "@nth_cc//nth/strings/format:universal",
    ],
)

cc_library(
    name = "token_buffer",
    hdrs = ["token_buffer.h"],
    srcs = ["token_buffer.cc"],
    deps = [
        "//common:resources",
        ":token",
        "//diagnostics/consumer",
        "@nth_cc//nth/container:flyweight_set",
        "@nth_cc//nth/debug",
    ],
)
