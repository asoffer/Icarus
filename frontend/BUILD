package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "lex",
    srcs = ["lex.cc"],
    deps = [
        ":numbers",
        ":tagged_node",
        ":text_span",
        ":token",
        "//ast:hole",
        "//ast:identifier",
        "//ast:terminal",
        "//error:log",
        "//type:primitive",
    ],
)

cc_lib(
    name = "numbers",
    hdrs = ["numbers.h"],
    srcs = ["numbers.cc"],
    deps = [
        "//base:types",
        "//base:debug",
    ],
)

cc_lib(
    name = "operators",
    hdrs = ["operators.h"],
    textual_hdrs = ["operators.xmacro.h"],
)

cc_lib(
    name = "parse",
    srcs = ["parse.cc"],
    deps = [
        ":tagged_node",
        ":token",
        "//ast",
    ],
)

cc_lib(
    name = "source",
    hdrs = ["source.h"],
    srcs = ["source.cc"],
    deps = ["//base/container:vector"],
)

cc_lib(
    name = "tagged_node",
    hdrs = ["tagged_node.h"],
    srcs = ["tagged_node.h"],
)

cc_lib(
    name = "text_span",
    hdrs = ["text_span.h"],
    srcs = ["text_span.cc"],
    deps = [
        ":source",
        "//base:debug",
        "//base:interval",
        "//base:types",
    ],
)

cc_lib(
    name = "token",
    hdrs = ["token.h"],
    deps = [
        ":operators",
        "//ast:node",
    ]
)
