package(default_visibility = ["//visibility:public"])

cc_library(
    name = "basic",
    hdrs = ["basic.h"],
    srcs = ["basic.cc"],
    deps = [
        "//ir:byte_code_writer",
        "//ir/instruction:base",
        "//ir/instruction:op_codes",
        "//ir/instruction:jump",
        "//base:ptr_span",
        "//base:untyped_buffer",
        "//type:type",
    ],
)

cc_library(
    name = "group",
    hdrs = ["group.h"],
    srcs = ["group.cc"],
    deps = [
        ":basic",
        ":stack_frame_allocations",
        "//ast:ast_fwd",
        "//base:ptr_span",
        "//base:strong_types",
        "//core:alignment",
        "//core:bytes",
        "//core:params",
        "//core:params_ref",
        "//type:pointer",
        "//type:type_fwd",
        "//type:typed_value",
    ],
)

cc_library(
    name = "stack_frame_allocations",
    hdrs = ["stack_frame_allocations.h"],
    deps = [
        "//base:stringify",
        "//ir/value:addr",
        "//ir/value:reg",
        "//type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)
