package(default_visibility = ["//visibility:public"])

cc_library(
    name = "char_range",
    hdrs = ["char_range.h"],
    deps = [
        "//base:debug",
    ],
)

cc_library(
    name = "lex",
    hdrs = ["lex.h"],
    srcs = ["lex.cc"],
    deps = [
        ":char_range",
        "//base:debug",
        "//diagnostic/consumer",
    ],
)

cc_binary(
    name = "lex_fuzz_test",
    srcs = ["lex_fuzz_test.cc"],
    deps = [
        ":lex",
        "//diagnostic/consumer:trivial",
        "//frontend/source:buffer",
        "//test:fuzz",
    ],
    copts = ["-fsanitize=fuzzer"],
    linkopts = ["-fsanitize=fuzzer"],
    testonly = True,
)
