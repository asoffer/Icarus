package(default_visibility = ["//visibility:public"])

cc_library(
    name = "assignment_and_initialization",
    hdrs = ["assignment_and_initialization.h"],
    srcs = ["assignment_and_initialization.cc"],
    deps = [
        "//compiler:common_diagnostics",
        "//compiler/verify:common",
        "//diagnostic/consumer",
        "//frontend/source:buffer",
        "//type:cast",
        "//type:qual_type",
    ],
)

cc_test(
    name = "assignment_and_initialization_test",
    srcs = ["assignment_and_initialization_test.cc"],
    deps = [
        ":assignment_and_initialization",
        "//diagnostic/consumer:trivial",
        "//type:pointer",
        "@com_google_googletest//:gtest_main",
    ],
)
