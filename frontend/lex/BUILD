package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

# TODO handle match
cc_lib(
    name = "lex",
    deps = [
        ":lexeme",
        ":numbers",
        "//ast:ast",
        "//ast:hashtag",
        "//core:builtin",
        "//diagnostic/consumer",
        "//diagnostic:errors",
        "//frontend/source:cursor",
        "//ir:results",
        "//ir:str",
        "//type:basic_type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_binary(
    name = "lex_fuzz_test",
    srcs = ["lex_fuzz_test.cc"],
    deps = [
        ":lex",
        "//diagnostic/consumer:trivial",
        "//frontend/source:cursor",
        "//frontend/source:string",
        "//test:fuzz",
    ],
    copts = ["-fsanitize=fuzzer"],
    linkopts = ["-fsanitize=fuzzer"],
    testonly = True,
)

cc_library(
    name = "lexeme",
    hdrs = ["lexeme.h"],
    deps = [
        ":operators",
        ":syntax",
        ":tag",
        "//ast:node",
        "//base:meta",
    ],
)

cc_lib(
    name = "numbers",
    deps = ["//base:expected"],
    test_deps = None,
)

cc_test(
    name = "numbers_test",
    srcs = ["numbers_test.cc"],
    deps = [
        ":numbers",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_lib(
    name = "operators",
    textual_hdrs = ["operators.xmacro.h"],
    deps = [":tag", "//base:debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "syntax",
    textual_hdrs = ["syntax.xmacro.h"],
    deps = [":tag", "//base:debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "tag",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "tagged_node",
    deps = [":tag", ":lexeme"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "token",
    deps = [
        ":operators",
        "//ast:node",
        "//base:debug",
    ],
    header_only = True,
    test_deps = None,
)
