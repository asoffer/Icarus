package(default_visibility = ["//visibility:public"])

cc_library(
    name = "any_func",
    hdrs = ["any_func.h"],
    deps = [
        "//base:debug",
        "//ir/value:foreign_fn",
    ],
)

cc_test(
    name = "any_func_test",
    srcs = ["any_func_test.cc"],
    deps = [
        ":any_func",
        ":compiled_fn",
        "//type:type",
        "//type:function",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "basic_block",
    hdrs = ["basic_block.h"],
    srcs = ["basic_block.cc"],
    deps = [
        ":byte_code_writer",
        ":instructions_base",
        ":instruction_op_codes",
        "//ir/cmd:jump",
        "//base:untyped_buffer",
        "//type:type",
    ],
)

cc_library(
    name = "block_def",
    hdrs = ["block_def.h"],
    srcs = ["block_def.cc"],
    deps = [
        ":overload_set",
        "//base:debug",
    ],
)

cc_library(
    name = "block_group",
    hdrs = ["block_group.h"],
    srcs = ["block_group.cc"],
    deps = [
        ":basic_block",
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
        ":block_group",
        "//ast:ast",
        "//base:move_func",
        "//core:params",
        "//type:jump",
        "//type:qual_type",
    ],
)

cc_library(
    name = "builder",
    hdrs = ["builder.h"],
    srcs = ["builder.cc"],
    deps = [
        ":basic_block",
        ":block_group",
        ":instructions",
        ":local_block_interpretation",
        ":struct_field",
        "//base:debug",
        "//base:scope",
        "//base:tag",
        "//ir/value:addr",
        "//ir/value:reg",
        "//type:typed_value",
        "//type:util",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "instruction_inliner",
    hdrs = ["instruction_inliner.h"],
    srcs = ["instruction_inliner.cc"],
    deps = [
        ":basic_block",
        ":block_group",
        ":instructions_base",
        ":local_block_interpretation",
        ":results",
        "//base:debug",
        "//ir/cmd:jump",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ]
)

cc_test(
    name = "instruction_inliner_test",
    srcs = ["instruction_inliner_test.cc"],
    deps = [
        ":builder",
        ":compiled_fn",
        ":jump",
        ":instruction_inliner",
        "//type",
        "//type:function",
        "//type:jump",
        "//test",
    ],
)

cc_library(
    name = "instruction_op_codes",
    hdrs = ["instruction_op_codes.h"],
)

cc_library(
    name = "instructions_base",
    hdrs = ["instructions_base.h"],
    deps = [
        "//base:cast",
        "//base:clone",
        "//base:untyped_buffer",
    ]
)
cc_library(
    name = "instructions",
    hdrs = ["instructions.h", "instructions_base.h"],
    deps = [
        ":block_def",
        ":instructions_base",
        ":instruction_inliner",
        ":instruction_op_codes",
        ":out_params",
        ":scope_def",
        ":struct_field",
        "//ast/scope",
        "//base:clone",
        "//ir/value:reg_or",
        "//ir/value:string",
        "//type",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
        "//type:variant",
        "@com_google_absl//absl/strings",
    ],
)

# TODO now that ':builtin'; doesn't need any IR, move it up to ast and rename
# this to ':builtin'
cc_library(
    name = "builtin_ir",
    hdrs = ["builtin_ir.h"],
    srcs = ["builtin_ir.cc"],
    deps = [
        ":any_func",
        ":builder",
        ":compiled_fn",
    ],
)

cc_library(
    name = "components",
    hdrs = ["components.h"],
    srcs = ["components.cc"],
    deps = [
        ":builder",
        ":compiled_fn",
    ],
)

cc_library(
    name = "compiled_fn",
    hdrs = ["compiled_fn.h"],
    srcs = ["compiled_fn.cc"],
    deps = [
        ":basic_block",
        ":block_group",
        "//ast:ast",
        "//base:move_func",
        "//base:ptr_span",
        "//base:scope",
        "//core:arch",
        "//core:params",
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
        ":basic_block",
        ":builder",
        ":compiled_fn",
        ":instruction_inliner",
        ":jump",
        ":local_block_interpretation",
        ":results",
        ":stack_frame_allocations",
        "//base:macros",
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
        ":basic_block",
        "//ast:ast",
        "//base:macros",
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
        "//test",
    ],
)

cc_library(
    name = "scope_def",
    hdrs = ["scope_def.h"],
    deps = [
        ":overload_set",
        "//base:move_func",
        "//type",
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

cc_library(
    name = "str",
    hdrs = ["str.h"],
    srcs = ["str.cc"],
    deps = [
        ":read_only_data",
        "//base:guarded",
        "//base:untyped_buffer",
        "//ir/value:addr",
        "@com_google_absl//absl/container:node_hash_map",
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
        ":any_func",
        ":compiled_fn", # Remove once AnyFunc depends on this.
        "//core:params_ref",
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
        ":any_func",
        ":overload_set",
        "//ir/value:foreign_fn",
        "//type:type",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ]
)
