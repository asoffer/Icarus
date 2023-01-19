package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parse_rule",
    hdrs = ["parse_rule.h"],
    deps = [
        "//ast:node",
        "//diagnostic/consumer",
        "//frontend/lex:tag",
    ],
    test_deps = None,
)

cc_library(
    name = "parse",
    hdrs = ["parse.h"],
    srcs = ["parse.cc"],
    deps = [
        ":parse_rule",
        "//ast:ast",
        "//ast:ast_fwd",
        "//base:global",
        "//diagnostic/consumer",
        "//frontend/lex:lex",
        "//frontend/lex:tagged_node",
        "//frontend/lex:token",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
    test_deps = None,
)

cc_binary(
    name = "parse_fuzz_test",
    srcs = ["parse_fuzz_test.cc"],
    deps = [
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
