package(default_visibility = ["//visibility:public"])

cc_library(
    name = "consumer",
    hdrs = ["consumer.h"],
    srcs = ["consumer.cc"],
    deps = [
        "//lexer:lex",
        "//lexer:token",
        "//parse:tree",
        "//diagnostics:message",
        "@nth_cc//nth/base:attributes",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "null",
    hdrs = ["null.h"],
    deps = [
        ":consumer",
    ],
)

cc_library(
    name = "streaming",
    hdrs = ["streaming.h"],
    srcs = ["streaming.cc"],
    deps = [
        ":consumer",
        "//diagnostics:message",
        "@com_google_absl//absl/strings:str_format",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/strings:text",
    ],
)
