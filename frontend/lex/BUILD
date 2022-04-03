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
        "//core:lexeme",
        "//diagnostic/consumer",
        "@com_google_absl//absl/types:span",
    ],
)

cc_binary(
    name = "lex_fuzz_test",
    srcs = ["lex_fuzz_test.cc"],
    deps = [
        ":lex",
        "//diagnostic/consumer:trivial",
        "//test:fuzz",
    ],
    copts = ["-fsanitize=fuzzer"],
    linkopts = ["-fsanitize=fuzzer"],
    testonly = True,
)

cc_library(
    name = "operators",
    hdrs = ["operators.h"],
    textual_hdrs = ["operators.xmacro.h"],
    deps = [":tag", "//base:debug"],
)

cc_library(
    name = "tag",
    hdrs = ["tag.h"],
    deps = [],
)
