package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parse_rule",
    hdrs = ["parse_rule.h"],
    deps = [
        "//ast:node",
        "//base:no_destructor",
        "//diagnostic/consumer",
        "//frontend/lex:tag",
        "@com_google_absl//absl/types:span",
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
        "//frontend/source:source",
        "@com_google_absl//absl/container:flat_hash_map",
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
        "//frontend/source:cursor",
        "//frontend/source:string",
    ],
    copts = ["-fsanitize=fuzzer"],
    linkopts = ["-fsanitize=fuzzer"],
    testonly = True,
)
