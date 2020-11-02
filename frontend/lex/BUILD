package(default_visibility = ["//visibility:public"])

# TODO handle match
cc_library(
    name = "lex",
    hdrs = ["lex.h"],
    srcs = ["lex.cc"],
    deps = [
        ":lexeme",
        ":numbers",
        "//ast:ast",
        "//base:global",
        "//diagnostic/consumer",
        "//frontend/source:cursor",
        "//ir/value:builtin_fn",
        "//ir/value:hashtag",
        "//ir/value:string",
        "//type:primitive",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
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

cc_library(
    name = "numbers",
    hdrs = ["numbers.h"],
    srcs = ["numbers.cc"],
    deps = ["//base:expected"],
)

cc_test(
    name = "numbers_test",
    srcs = ["numbers_test.cc"],
    deps = [
        ":numbers",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "operators",
    hdrs = ["operators.h"],
    textual_hdrs = ["operators.xmacro.h"],
    deps = [":tag", "//base:debug"],
)

cc_library(
    name = "syntax",
    hdrs = ["syntax.h"],
    textual_hdrs = ["syntax.xmacro.h"],
    deps = [":tag", "//base:debug"],
)

cc_library(
    name = "tag",
    hdrs = ["tag.h"],
    deps = [],
)

cc_library(
    name = "tagged_node",
    hdrs = ["tagged_node.h"],
    deps = [
        ":tag",
        ":lexeme",
        "//base:meta",
    ],
)

cc_library(
    name = "token",
    hdrs = ["token.h"],
    deps = [
        ":operators",
        "//ast:node",
        "//base:debug",
    ],
)
