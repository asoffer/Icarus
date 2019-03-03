package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_target(
    name = "lex",
    intf_deps = [
        ":lexeme",
        ":source",
        ":text_span",
        "//error:log",
    ],
    impl_deps = [
        ":numbers",
        "//ast:builtin_fn",
        "//ast:hole",
        "//ast:identifier",
        "//ast:terminal",
        "//ir:builtin",
        "//ir:results",
        "//type:primitive",
    ],
)

cc_lib(
    name = "lexeme",
    hdrs = ["lexeme.h"],
    deps = [
        ":operators",
        ":syntax",
        ":tag",
    ],
)

cc_lib(
    name = "numbers",
    hdrs = ["numbers.h"],
    srcs = ["numbers.cc"],
    deps = ["//base:expected"]
)

cc_lib(
    name = "operators",
    hdrs = ["operators.h"],
    textual_hdrs = ["operators.xmacro.h"],
    deps = [
        ":tag",
        "//base:debug",
    ],
)

cc_lib(
    name = "syntax",
    hdrs = ["syntax.h"],
    textual_hdrs = ["syntax.xmacro.h"],
    deps = [
        ":tag",
        "//base:debug",
    ],
)

cc_lib(
    name = "parse",
    srcs = ["parse.cc"],
    deps = [
        ":lex",
        ":source",
        ":tagged_node",
        ":token",
        "//ast",
        "//base:guarded",
    ],
)

cc_lib(
    name = "source",
    hdrs = ["source.h"],
    srcs = ["source.cc"],
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
    name = "tag",
    hdrs = ["tag.h"],
)

cc_lib(
    name = "tagged_node",
    hdrs = ["tagged_node.h"],
    srcs = ["tagged_node.h"],
    deps = [":tag", ":lexeme"],
)

cc_lib(
    name = "text_span",
    hdrs = ["text_span.h"],
    deps = [
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
