package(default_visibility = ["//visibility:public"])

cc_library(
    name = "assignment_and_initialization",
    hdrs = ["assignment_and_initialization.h"],
    srcs = ["assignment_and_initialization.cc"],
    deps = [
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

cc_library(
    name = "qual_type_iterator",
    hdrs = ["qual_type_iterator.h"],
    deps = [
        "//type:qual_type",
        "//type:type",
    ],
)

# TODO Tests for qual_type_iterator
