package(default_visibility = ["//visibility:public"])

cc_library(
    name = "addr",
    hdrs = ["addr.h"],
    srcs = ["addr.cc"],
    deps = [
        "//core:bytes",
        "//base:debug",
    ],
)

cc_test(
    name = "addr_test",
    srcs = ["addr_test.cc"],
    deps = [
        ":addr",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "enum_and_flags",
    hdrs = ["enum_and_flags.h"],
    deps = [
        "//base:strong_types",
    ],
)

cc_test(
    name = "enum_and_flags_test",
    srcs = ["enum_and_flags_test.cc"],
    deps = [
        ":enum_and_flags",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "label",
    hdrs = ["label.h"],
    deps = ["//base:strong_types"],
)

cc_library(
    name = "reg",
    hdrs = ["reg.h"],
    srcs = ["reg.cc"],
    deps = [
        "//base:debug",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "reg_test",
    srcs = ["reg_test.cc"],
    deps = [
        ":reg",
        "//base:stringify",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "reg_or",
    hdrs = ["reg_or.h"],
    deps = [":reg"],
)

cc_test(
    name = "reg_or_test",
    srcs = ["reg_or_test.cc"],
    deps = [
        ":reg_or",
        "//base:stringify",
        "@com_google_googletest//:gtest_main",
    ],
)
