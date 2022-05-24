package(default_visibility = ["//visibility:public"])

cc_library(
    name = "basic_block",
    hdrs = ["basic_block.h"],
    srcs = ["basic_block.cc"],
    deps = [
        ":subroutine_cc_proto",
        "//base:ptr_span",
        "//base:untyped_buffer",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//ir/instruction:base",
        "//ir/instruction:jump",
    ],
)

cc_library(
    name = "out_params",
    hdrs = ["out_params.h"],
    deps = [
        "//base:extend",
        "//base/extend:serialize",
        "//ir/value:reg",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":subroutine",
        "//base:any_invocable",
        "//base:debug",
        "//base:iterator",
        "//ir/byte_code",
        "//ir/value:fn",
        "//ir/value:scope",
        "//type:function",
        "//type:pointer",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)

cc_library(
    name = "subroutine",
    hdrs = ["subroutine.h"],
    srcs = ["subroutine.cc"],
    deps = [
        ":subroutine_cc_proto",
        "//base:ptr_span",
        "//base:strong_types",
        "//core:alignment",
        "//core:bytes",
        "//core:parameters",
        "//ir:basic_block",
        "//ir/blocks:register_allocator",
        "//type:callable",
        "//type:system",
    ],
)

proto_library(
    name = "subroutine_proto",
    srcs = ["subroutine.proto"],
    deps = ["//ir/instruction:instruction_proto"],
)

cc_proto_library(
    name = "subroutine_cc_proto",
    deps = [":subroutine_proto"],
)
