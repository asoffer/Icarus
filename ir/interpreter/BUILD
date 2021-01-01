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
    name = "register_array",
    hdrs = ["register_array.h"],
    deps = [
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//ir/value",
        "//ir/value:reg",
    ],
)

cc_library(
    name = "evaluate",
    hdrs = ["evaluate.h"],
    srcs = ["evaluate.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":architecture",
        ":evaluation_failure",
        ":execution_context",
        "//base:debug",
        "//base:expected",
        "//base:untyped_buffer",
        "//core:arch",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//ir/instruction",
        "//ir/instruction:arithmetic",
        "//ir/instruction:byte_view",
        "//ir/instruction:compare",
        "//ir/instruction:core",
        "//ir/instruction:set",
        "//ir/value",
        "//ir/value:char",
        "//ir/value:generic_fn",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:opaque",
        "//type:pointer",
        "//type:slice",
        "//type:struct",
        "//type:generic_struct",
        "//type:generic_function",
    ],
)

cc_library(
    name = "evaluation_failure",
    hdrs = ["evaluation_failure.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//diagnostic:message",
        "//frontend/source:buffer",
    ],
)

cc_library(
    name = "stack_frame",
    hdrs = ["stack_frame.h"],
    srcs = ["stack_frame.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":architecture",
        ":register_array",
        "//base:untyped_buffer_view",
        "//core:arch",
        "//ir/blocks:basic",
        "//ir/value:native_fn",
        "//type:type",
    ],
)

cc_library(
    name = "execution_context",
    hdrs = ["execution_context.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":foreign",
        ":stack_frame",
        "//base:untyped_buffer",
        "//ir:read_only_data",
        "//ir/value",
        "//ir/value:addr",
        "//ir/value:fn",
        "//ir/value:reg",
        "//ir/value:reg_or",
    ],
)

cc_library(
    name = "foreign",
    hdrs = ["foreign.h"],
    srcs = ["foreign.cc"],
    visibility = ["//visibility:public"],
    deps = [
        "//base:debug",
        "//base:expected",
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//ir:read_only_data",
        "//ir/value",
        "//ir/value:addr",
        "//ir/value:foreign_fn",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "@com_google_absl//absl/types:span",
    ],
)
