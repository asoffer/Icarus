package(default_visibility = ["//visibility:public"])

cc_library(
    name = "block_def",
    hdrs = ["block_def.h"],
    deps = [
        "//base:debug",
        "//ir/value:overload_set",
    ],
)

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
    name = "jump",
    hdrs = ["jump.h"],
    deps = [
        "//ast:ast",
        "//core:params",
        "//ir/blocks:group",
        "//type:jump",
        "//type:qual_type",
    ],
)

cc_library(
    name = "builder",
    hdrs = ["builder.h"],
    srcs = ["builder.cc"],
    deps = [
        ":local_block_interpretation",
        ":struct_field",
        "//base:debug",
        "//base:scope",
        "//base:tag",
        "//ir/blocks:basic",
        "//ir/blocks:group",
        "//ir/instruction",
        "//ir/value:addr",
        "//ir/value:reg",
        "//type:typed_value",
        "//type:util",
        "@com_google_absl//absl/types:span",
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
    name = "inliner",
    hdrs = ["inliner.h"],
    srcs = ["inliner.cc"],
    deps = [
        ":builder",
        ":compiled_fn",
        ":jump",
        ":local_block_interpretation",
        ":results",
        "//base:macros",
        "//ir/instruction:inliner",
        "//ir/blocks:basic",
        "//ir/value:reg",
        "//type:function",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
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
    deps = ["//base:untyped_buffer"],
)

cc_library(
    name = "results",
    hdrs = ["results.h"],
    srcs = ["results.cc"],
    deps = [
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//core:bytes",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "results_test",
    srcs = ["results_test.cc"],
    deps = [
        ":results",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "scope_def",
    hdrs = ["scope_def.h"],
    deps = [
        "//base:move_func",
        "//base:no_destructor",
        "//ir/value:overload_set",
        "//type",
    ],
)

cc_library(
    name = "struct_field",
    hdrs = ["struct_field.h"],
    deps = [
        "//ir/value:reg_or",
        "//type",
    ],
)
