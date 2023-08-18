package(default_visibility = ["//visibility:public"])


cc_library(
    name = "common_diagnostics",
    hdrs = ["common_diagnostics.h"],
    deps = [
        "//diagnostic:message",
    ]
)

cc_library(
    name = "compiler",
    hdrs = ["compiler.h"],
    srcs = [
        "slice_type.cc",
        "terminal.cc",
    ],
    deps = [
        "//ast",
        "//core:call",
        "//diagnostic/consumer",
        "//module",
        "//module:resources",
        "//semantic_analysis:context",
        "//semantic_analysis/type_verification:diagnostics",
        "//semantic_analysis:function_data",
        "//semantic_analysis:type_system",
        "@asoffer_nth//nth/coroutine",
        "@asoffer_nth//nth/utility:buffer",
    ],
)

cc_test(
    name = "slice_type_test",
    srcs = ["slice_type_test.cc"],
    deps = [
        ":compiler",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "terminal_test",
    srcs = ["terminal_test.cc"],
    deps = [
        ":compiler",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)
