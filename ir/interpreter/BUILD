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
    name = "evaluate",
    hdrs = ["evaluate.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":architecture",
        ":execution_context",
        "//base:debug",
        "//base:untyped_buffer",
        "//core:arch",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//ir/instruction",
        "//ir/value:result_buffer",
        "@com_google_absl//absl/cleanup",
    ],
)

cc_library(
    name = "stack_frame",
    hdrs = ["stack_frame.h"],
    srcs = ["stack_frame.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":architecture",
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//core:arch",
        "//ir/blocks:basic",
        "//ir/value:native_fn",
        "//ir/value:reg",
        "//type:type",
    ],
)

cc_library(
    name = "execution_context",
    hdrs = ["execution_context.h"],
    srcs = ["execution_context.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":foreign",
        ":stack_frame",
        "//base:untyped_buffer",
        "//ir/byte_code:reader",
        "//ir/instruction:core",
        "//ir/value:addr",
        "//ir/value:fn",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type:pointer",
        "@com_google_absl//absl/cleanup",
    ],
)

cc_library(
    name = "foreign",
    hdrs = ["foreign.h"],
    srcs = ["foreign.cc"],
    visibility = ["//visibility:public"],
    deps = [
        "//base:debug",
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//ir/value:addr",
        "//ir/value:foreign_fn",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
    ],
)
