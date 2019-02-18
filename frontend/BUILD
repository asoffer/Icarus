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
    deps = ["//base:debug"]
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
        "//base:guarded",
    ],
)

cc_lib(
    name = "deprecated_source",
    hdrs = ["deprecated_source.h"],
    srcs = ["deprecated_source.cc"],
)

cc_lib(
    name = "source",
    hdrs = ["source.h"],
    deps = ["//base:expected"],
)

cc_test(
    name = "source_test",
    srcs = ["source_test.cc"],
    deps = [
        "//test",
        ":source",
    ],
    data= [
        "testdata/empty_file.txt",
        "testdata/one_line_file.txt",
        "testdata/multi_line_file.txt",
    ],
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
        ":deprecated_source",
        "//base:debug",
        "//base:interval",
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
