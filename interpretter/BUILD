package(default_visibility = ["//visibility:public"])

cc_library(
    name = "architecture",
    hdrs = ["architecture.h"],
    deps = [
        "//core:arch",
        "//ir:addr",
        "//ir:any_func",
    ],
)

cc_library(
    name = "register_array",
    hdrs = ["register_array.h"],
    deps = [
        "//base:untyped_buffer",
        "//ir:reg",
    ],
)

cc_library(
    name = "evaluate",
    hdrs = ["evaluate.h"],
    srcs = ["evaluate.cc"],
    deps = [
        ":architecture",
        ":execute",
        "//base:debug",
        "//base:untyped_buffer",
        "//core:arch",
        "//ir:compiled_fn",
        "//ir:results",
        "//type:function",
    ],
)

cc_library(
    name = "stack_frame",
    hdrs = ["stack_frame.h"],
    srcs = ["stack_frame.cc"],
    deps = [
        ":architecture",
        ":register_array",
        "//core:arch",
        "//ir:basic_block",
        "//ir:compiled_fn",
        "//type:type",
    ],
)

cc_library(
    name = "execute",
    hdrs = ["execute.h"],
    srcs = ["execute.cc"],
    deps = [
        ":architecture",
        ":foreign",
        ":stack_frame",
        "//base:meta",
        "//base:untyped_buffer",
        "//ir:addr",
        "//ir:any_func",
        "//ir:block_def",
        "//ir:instructions",
        "//ir:jump",
        "//ir:read_only_data",
        "//ir:reg",
        "//ir:reg_or",
        "//ir:scope_def",
        "//type:opaque",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "foreign",
    hdrs = ["foreign.h"],
    srcs = ["foreign.cc"],
    deps = [
        "//base:debug",
        "//base:untyped_buffer",
        "//ir:addr",
        "//ir:foreign_fn",
        "//ir:read_only_data",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "@com_google_absl//absl/types:span",
    ],
)
