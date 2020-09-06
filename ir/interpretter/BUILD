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
        ":execute",
        "//base:debug",
        "//base:expected",
        "//base:untyped_buffer",
        "//core:arch",
        "//ir:compiled_fn",
        "//ir/instruction:set",
        "//ir:jump",
        "//ir/value",
        "//ir/value:generic_fn",
        "//type:function",
        "//type:generic_struct",
        "//type:generic_function",
    ],
)

cc_library(
    name = "evaluation_failure",
    hdrs = ["evaluation_failure.h"],
    visibility = ["//visibility:public"],
    deps = [],
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
    name = "execute_header",
    hdrs = ["execute.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":foreign",
        ":stack_frame",
        "//base:untyped_buffer",
        "//ir/value:addr",
        "//ir/value:fn",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "execute",
    srcs = ["execute.cc"],
    deps = [
        ":execute_header",
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
        "//ir:read_only_data",
        "//ir/value:addr",
        "//ir/value:foreign_fn",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "//type:tuple",
        "@com_google_absl//absl/types:span",
    ],
)
