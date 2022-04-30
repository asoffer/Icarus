package(default_visibility = ["//visibility:private"])

cc_library(
    name = "architecture",
    hdrs = ["architecture.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//core:arch",
        "//ir/value:addr",
        "//ir/value:fn",
    ],
)

cc_library(
    name = "stack_frame",
    hdrs = ["stack_frame.h"],
    srcs = ["stack_frame.cc"],
    visibility = ["//visibility:public"],
    deps = [
        "//base:untyped_buffer",
        "//core:bytes",
        "//ir/value:addr",
        "//ir/value:reg",
        "//ir/value:reg_or",
    ],
)

cc_test(
    name = "stack_frame_test",
    srcs = ["stack_frame_test.cc"],
    deps = [
        ":stack_frame",
        "//core:bytes",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "interpreter",
    hdrs = ["interpreter.h"],
    srcs = ["interpreter.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":architecture",
        ":stack_frame",
        "//base:macros",
        "//base:untyped_buffer",
        "//core:arch",
        "//core:bytes",
        "//core:type_contour",
        "//ir:subroutine",
        "//ir/instruction:base",
        "//ir/value:result_buffer",
        "//module:shared_context",
        "@com_google_absl//absl/cleanup",
    ],
)
