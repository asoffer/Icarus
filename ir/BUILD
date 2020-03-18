package(default_visibility = ["//visibility:public"])

cc_library(
    name = "block_def",
    hdrs = ["block_def.h"],
    deps = [
        ":overload_set",
        "//base:debug",
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
        "//base:move_func",
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
    srcs = ["compiled_fn.cc"],
    deps = [
        "//ast:ast",
        "//base:move_func",
        "//base:ptr_span",
        "//base:scope",
        "//core:arch",
        "//core:params",
        "//ir/blocks:basic",
        "//ir/blocks:group",
        "//type:function",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
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
        "//ir/blocks:stack_frame_allocations",
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
        ":overload_set",
        "//base:move_func",
        "//base:no_destructor",
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

cc_library(
    name = "overload_set",
    hdrs = ["overload_set.h"],
    deps = [
        "//core:params_ref",
        "//ir/value:fn",
        "//type:cast",
        "//type:function",
        "//type:qual_type",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "overload_set_test",
    srcs = ["overload_set_test.cc"],
    deps = [
        ":overload_set",
        "//ir/value:fn",
        "//type:type",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ]
)
