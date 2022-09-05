package(default_visibility = ["//visibility:public"])

cc_library(
    name = "verify",
    hdrs = ["verify.h"],
    srcs = [
        "declaration.cc",
        "identifier.cc",
        "short_function_literal.cc",
        "unary_operator.cc",
        "terminal.cc",
    ],
    deps = [
        "//ast",
        "//base:meta",
        "//compiler:common",
        "//compiler:common_diagnostics",
        "//compiler:context",
        "//compiler:type_for_diagnostic",
        "//diagnostic/consumer",
        "//semantic_analysis:task",
        "//type",
        "//type:array",
        "//type:overload_set",
        "//type:primitive",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "matchers",
    hdrs = ["matchers.h"],
    testonly = True,
    deps = [
        ":verify",
        "//compiler:context",
        "//diagnostic/consumer:tracking",
        "//frontend:parse",
        "//ir:module",
        "//ir/value:module_id",
        "//type:qual_type",
        "@com_google_googletest//:gtest",
    ],
)

cc_test(
    name = "short_function_literal_test",
    srcs = ["short_function_literal_test.cc"],
    deps = [
        ":matchers",
        ":verify",
        "//type:primitive",
        "//type:function",
        "//type:qual_type",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "unary_operator_test",
    srcs = ["unary_operator_test.cc"],
    deps = [
        ":matchers",
        ":verify",
        "//type:primitive",
        "//type:function",
        "//type:qual_type",
        "@com_google_googletest//:gtest_main",
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
