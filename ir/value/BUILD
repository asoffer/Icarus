package(default_visibility = ["//visibility:public"])

cc_library(
    name = "addr",
    hdrs = ["addr.h"],
)

cc_library(
    name = "block",
    hdrs = ["block.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "char",
    hdrs = ["char.h"],
    deps = [
        "//base:serialize",
    ],
)

cc_library(
    name = "fn",
    hdrs = ["fn.h"],
    deps = [
        ":module_id",
        "//base:extend",
        "//base/extend:absl_hash",
        "//base/extend:serialize",
    ],
)

cc_library(
    name = "hashtag",
    hdrs = ["hashtag.h"],
)

cc_library(
    name = "integer",
    hdrs = ["integer.h"],
    deps = [
        "//base:serialize",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "label",
    hdrs = ["label.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "module_id",
    hdrs = ["module_id.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "reg",
    hdrs = ["reg.h"],
    deps = [
        "//base:debug",
        "//base:extend",
        "//base/extend:absl_hash",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "reg_test",
    srcs = ["reg_test.cc"],
    deps = [
        ":reg",
        "//base:universal_print",
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
        "//base:universal_print",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "result_buffer",
    hdrs = ["result_buffer.h"],
    srcs = ["result_buffer.cc"],
    deps = [
        ":addr",
        ":reg_or",
        "//base:meta",
        "//base:serialize",
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
    ]
)

cc_test(
    name = "result_buffer_test",
    srcs = ["result_buffer_test.cc"],
    deps = [
        ":result_buffer",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "slice",
    hdrs = ["slice.h"],
    deps = [
        ":addr",
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
        "//base/extend:compare",
    ],
)

#TODO: slice_test

cc_library(
    name = "matcher",
    hdrs = ["matcher.h"],
    deps = [
        ":result_buffer",
        "@com_google_googletest//:gtest",
    ],
    testonly = True,
)
