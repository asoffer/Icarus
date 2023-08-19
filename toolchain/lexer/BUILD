package(default_visibility = ["//visibility:public"])

cc_library(
    name = "lexer",
    hdrs = ["lexer.h"],
    srcs = ["lexer.cc"],
    deps = [
        ":token",
        ":token_buffer",
        "@asoffer_nth//nth/debug",
    ],
)

cc_test(
    name = "lexer_test",
    srcs = ["lexer_test.cc"],
    deps = [
        ":lexer",
        "@asoffer_nth//nth/test:main",
    ],
)

cc_library(
    name = "token_kind_xmacro",
    textual_hdrs = ["token_kind.xmacro.h"],
)

cc_library(
    name = "token",
    hdrs = ["token.h"],
    srcs = ["token.cc"],
    deps = [
        ":token_kind_xmacro",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/io:printer",
        "@asoffer_nth//nth/strings:interpolate",
        "@asoffer_nth//nth/strings/format:universal",
    ],
)

cc_test(
    name = "token_test",
    srcs = ["token_test.cc"],
    deps = [
        ":token",
        "@asoffer_nth//nth/test:main",
        "@asoffer_nth//nth/io:string_printer",
        "@asoffer_nth//nth/strings:interpolate",
        "@asoffer_nth//nth/strings/format:universal",
    ],
)

cc_library(
    name = "token_buffer",
    hdrs = ["token_buffer.h"],
    srcs = ["token_buffer.cc"],
    deps = [
        ":token",
        "@asoffer_nth//nth/container:flyweight_set",
        "@asoffer_nth//nth/debug",
    ],
)

# TODO
# cc_test(
#     name = "token_buffer_test",
#     srcs = ["token_buffer_test.cc"],
#     deps = [
#         ":token_buffer",
#         "@asoffer_nth//nth/test:main",
#     ],
# )
