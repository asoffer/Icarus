package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parser_dsl",
    hdrs = ["parser_dsl.h"],
    deps = [
        "//base:meta",
        "//core:lexeme",
        "//core:parser",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "parse",
    hdrs = ["parse.h"],
    srcs = ["parse.cc"],
    deps = [
        ":parser_dsl",
        "//ast:ast",
        "//ast:module",
        "//core:lexeme",
        "//diagnostic/consumer",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "parse_test",
    srcs = ["parse_test.cc"],
    deps = [
        ":parse",
        "//core:lexeme",
        "//diagnostic/consumer:trivial",
        "//frontend/lex",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_binary(
    name = "parse_fuzz_test",
    srcs = ["parse_fuzz_test.cc"],
    deps = [
        "//frontend/lex",
        ":parse",
        "//diagnostic/consumer:trivial",
    ],
    copts = ["-fsanitize=fuzzer"],
    linkopts = ["-fsanitize=fuzzer"],
    testonly = True,
)

cc_library(
    name = "source_indexer",
    hdrs = ["source_indexer.h"],
    srcs = ["source_indexer.cc"],
    deps = [
        "//base:debug",
        "//ir/value:module_id",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "source_indexer_test",
    srcs = ["source_indexer_test.cc"],
    deps = [
        ":source_indexer",
        "@com_google_googletest//:gtest_main",
    ],
)
