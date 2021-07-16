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
    name = "byte_code_reader",
    hdrs = ["byte_code_reader.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//base:serialize",
        "//base:untyped_buffer",
        "@com_google_absl//absl/types:span",
    ]
)

cc_library(
    name = "byte_code_writer",
    hdrs = ["byte_code_writer.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//base:debug",
        "//base:serialize",
        "//base:untyped_buffer",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ]
)

cc_library(
    name = "register_array",
    hdrs = ["register_array.h"],
    deps = [
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//ir/value:reg",
    ],
)

cc_library(
    name = "evaluate",
    hdrs = ["evaluate.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":architecture",
        ":evaluation_result",
        ":execution_context",
        "//base:debug",
        "//base:untyped_buffer",
        "//core:arch",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//ir/instruction",
        "//ir/value:result_buffer",
    ],
)

cc_library(
    name = "evaluation_result",
    hdrs = ["evaluation_result.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//diagnostic:message",
        "//diagnostic/consumer",
        "//frontend/source:buffer",
        "//ir/value:result_buffer",
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
    srcs = ["execution_context.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":foreign",
        ":stack_frame",
        "//base:untyped_buffer",
        "//ir/instruction:core",
        "//ir:read_only_data",
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
        "//ir:read_only_data",
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
