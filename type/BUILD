package(default_visibility = ["//visibility:public"])

cc_library(
    name = "type_kind_xmacro",
    textual_hdrs = ["type_kind.xmacro.h"],
)

cc_library(
    name = "type",
    hdrs = ["type.h"],
    srcs = ["type.cc"],
    deps = [
        ":type_kind_xmacro",
        "//lexer:token_kind_xmacro",
        "@asoffer_nth//nth/debug",
    ],
)

cc_test(
    name = "type_test",
    srcs = ["type_test.cc"],
    deps = [
        ":type",
        "@asoffer_nth//nth/test:main",
    ]
)
