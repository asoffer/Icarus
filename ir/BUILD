package(default_visibility = ["//visibility:public"])

cc_library(
    name = "byte_code_writer",
    hdrs = ["byte_code_writer.h"],
    deps = [
        "//base:debug",
        "//base:untyped_buffer",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "builder",
    hdrs = ["builder.h"],
    srcs = ["builder.cc"],
    deps = [
        ":local_block_interpretation",
        "//ast:overload_set",
        "//base:debug",
        "//base:scope",
        "//base:tag",
        "//ir/blocks:basic",
        "//ir/blocks:group",
        "//ir/instruction",
        "//ir/instruction:core",
        "//ir/value:addr",
        "//ir/value:reg",
        "//type:typed_value",
        "//type:util",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "compiled_block",
    hdrs = ["compiled_block.h"],
    deps = [
        "//ast",
        "//ir/instruction:set",
        "//ir/value:overload_set",
        "//ir/value:jump",
        "//type:jump",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "compiled_fn",
    hdrs = ["compiled_fn.h"],
    deps = [
        "//ir/blocks:group",
        "//type:function",
    ],
)

cc_library(
    name = "compiled_jump",
    hdrs = ["compiled_jump.h"],
    deps = [
        "//ast:ast",
        "//core:params",
        "//ir/blocks:group",
        "//ir/value:jump",
        "//type:jump",
        "//type:qual_type",
    ],
)

cc_library(
    name = "compiled_scope",
    hdrs = ["compiled_scope.h"],
    deps = [
        "//ir/value:block",
        "//ir/value:jump",
        "//ir/value:overload_set",
        "//type",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "local_block_interpretation",
    hdrs = ["local_block_interpretation.h"],
    srcs = [],
    deps = [
        "//ast:ast",
        "//base:macros",
        "//ir/blocks:basic",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "out_params",
    hdrs = ["out_params.h"],
    deps = [
        ":byte_code_writer",
        "//ir/value:reg",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "read_only_data",
    hdrs = ["read_only_data.h"],
    deps = [
        "//base:global",
        "//base:untyped_buffer",
    ],
)
