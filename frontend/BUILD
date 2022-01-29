package(default_visibility = ["//visibility:public"])

cc_library(
    name = "lexeme",
    hdrs = ["lexeme.h"],
    deps = ["//base:debug"],
)

cc_library(
    name = "parse",
    hdrs = ["parse.h"],
    srcs = ["parse.cc"],
    deps = [
        ":lexeme",
        "//ast:ast",
        "//ast:module",
        "//diagnostic/consumer",
        "@com_google_absl//absl/types:span",
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
