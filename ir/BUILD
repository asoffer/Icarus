package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "addr",
    deps = [
        "//core:bytes",
        "//base:debug",
    ],
    test_deps = [],
)

cc_lib(
    name = "any_func",
    deps = [
        ":foreign_fn",
        "//base:debug",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "basic_block",
    deps = [
        ":byte_code_writer",
        ":instructions_base",
        ":instruction_op_codes",
        "//ir/cmd:jump",
        "//base:untyped_buffer",
        "//type:type",
    ],
    test_deps = None,
)

cc_lib(
    name = "block_def",
    deps = [
        ":any_func",
        "//base:debug",
    ],
    test_deps = None,
)

cc_lib(
    name = "block_group",
    deps = [
        ":basic_block",
        ":stack_frame_allocations",
        "//ast:ast_fwd",
        "//base:ptr_span",
        "//base:strong_types",
        "//core:alignment",
        "//core:bytes",
        "//core:fn_params",
        "//type:pointer",
        "//type:type_fwd",
        "//type:typed_value",
    ],
    test_deps = None,
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

cc_lib(
    name = "jump",
    deps = [
        ":block_group",
        "//ast:ast",
        "//base:move_func",
        "//core:fn_params",
        "//type:jump",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "builder",
    deps = [
        ":addr",
        ":basic_block",
        ":block_group",
        ":instructions",
        ":local_block_interpretation",
        ":struct_field",
        ":reg",
        "//base:debug",
        "//base:scope",
        "//base:tag",
        "//type:typed_value",
        "//type:util",
        "@com_google_absl//absl/types:span",
    ],
    test_deps = None,
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
        ":reg",
        ":reg_or",
        "//base:debug",
        "//ir/cmd:jump",
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
        ":reg_or",
        ":scope_def",
        ":struct_field",
        "//ast/scope",
        "//base:clone",
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
cc_lib(
    name = "builtin_ir",
    deps = [
        ":any_func",
        ":builder",
        ":compiled_fn",
    ],
    test_deps = None,
)

cc_lib(
    name = "components",
    deps = [
        ":builder",
        ":compiled_fn",
    ],
    test_deps = None,
)

cc_lib(
    name = "values",
    deps = ["//base:strong_types"],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "foreign_fn",
    deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "compiled_fn",
    deps = [
        ":basic_block",
        ":block_group",
        "//ast:ast",
        "//base:move_func",
        "//base:ptr_span",
        "//base:scope",
        "//core:arch",
        "//core:fn_params",
        "//type:function",
        "//type:pointer",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    test_deps = None,
)

cc_lib(
    name = "inliner",
    deps = [
        ":basic_block",
        ":builder",
        ":compiled_fn",
        ":instruction_inliner",
        ":jump",
        ":local_block_interpretation",
        ":reg",
        ":results",
        ":stack_frame_allocations",
        "//base:macros",
        "//type:function",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_library(
    name = "out_params",
    hdrs = ["out_params.h"],
    deps = [
        ":byte_code_writer",
        ":reg",
    ],
)

cc_lib(
    name = "reg",
    deps = [
        "//base:debug",
        "@com_google_absl//absl/strings",
    ],
    test_deps = [],
)

cc_lib(
    name = "reg_or",
    deps = [":reg"],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "results",
    deps = [
        ":reg",
        ":reg_or",
        "//base:untyped_buffer",
        "//core:bytes",
        "@com_google_absl//absl/strings",
    ],
    test_deps = [],
)

cc_lib(
    name = "scope_def",
    deps = [
        ":any_func",
        "//base:move_func",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "stack_frame_allocations",
    deps = [
        ":addr",
        ":reg",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "str",
    deps = [
        ":addr",
        ":read_only_data",
        "//base:guarded",
        "//base:untyped_buffer",
        "@com_google_absl//absl/container:node_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "read_only_data",
    deps = ["//base:untyped_buffer"],
    header_only = True,
    test_deps = None,
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
    name = "struct_field",
    hdrs = ["struct_field.h"],
    deps = [
        ":reg_or",
        "//type",
    ],
)
