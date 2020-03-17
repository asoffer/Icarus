package(default_visibility = ["//visibility:public"])

cc_library(
    name = "architecture",
    hdrs = ["architecture.h"],
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
        "//base:untyped_buffer_view",
        "//core:arch",
        "//ir/blocks:basic",
        "//ir/value:native_fn",
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
        "//ir:block_def",
        "//ir:jump",
        "//ir:read_only_data",
        "//ir:scope_def",
        "//ir/instruction",
        "//ir/value:addr",
        "//ir/value:foreign_fn",
        "//ir/value:fn",
        "//ir/value:native_fn",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type:opaque",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "foreign",
    hdrs = ["foreign.h"],
    srcs = ["foreign.cc"],
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
        "@com_google_absl//absl/types:span",
    ],
)
