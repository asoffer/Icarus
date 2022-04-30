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

cc_library(
    name = "evaluate",
    hdrs = ["evaluate.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":architecture",
        ":execution_context",
        "//base:debug",
        "//base:untyped_buffer",
        "//core:arch",
        "//ir:subroutine",
        "//ir/instruction",
        "//ir/value:result_buffer",
        "//module:shared_context",
        "@com_google_absl//absl/cleanup",
    ],
)

cc_library(
    name = "legacy_stack_frame",
    hdrs = ["legacy_stack_frame.h"],
    srcs = ["legacy_stack_frame.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":architecture",
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//core:arch",
        "//core:type_contour",
        "//ir:basic_block",
        "//ir/byte_code",
        "//ir/byte_code:byte_code_view",
        "//ir/value:reg",
        "//type:type",
        "@com_google_absl//absl/cleanup",
    ],
)

cc_library(
    name = "execution_context",
    hdrs = ["execution_context.h"],
    srcs = ["execution_context.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":legacy_stack_frame",
        "//base:untyped_buffer",
        "//ir/byte_code:reader",
        "//ir/instruction:core",
        "//ir/value:addr",
        "//ir/value:fn",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//ir/value:scope",
        "//type:primitive",
        "//module:shared_context",
        "//type:pointer",
        "@com_google_absl//absl/cleanup",
    ],
)
