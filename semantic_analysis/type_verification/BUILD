package(default_visibility = ["//visibility:public"])

cc_library(
    name = "verify",
    hdrs = ["verify.h"],
    srcs = [
        "terminal.cc",
        "short_function_literal.cc",
    ],
    deps = [
        "//ast",
        "//base:meta",
        "//compiler:context",
        "//semantic_analysis:task",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "terminal_test",
    srcs = ["terminal_test.cc"],
    deps = [
        ":matchers",
        ":verify",
        "//ir/value:slice",
        "//type:primitive",
        "//type:qual_type",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "matchers",
    hdrs = ["matchers.h"],
    testonly = True,
    deps = [
        ":verify",
        "//compiler:context",
        "//ir:module",
        "//ir/value:module_id",
        "//type:qual_type",
        "@com_google_googletest//:gtest",
    ],
)
