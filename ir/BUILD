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
        ":instructions",
        ":out_params",
        "//base:untyped_buffer",
        "//ir/cmd:basic",
        "//ir/cmd:call",
        "//ir/cmd:cast",
        "//ir/cmd:jump",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:print",
        "//ir/cmd:register",
        "//ir/cmd:return",
        "//ir/cmd:scope",
        "//ir/cmd:store",
        "//ir/cmd:types",
        "//ir/cmd:cmd_xmacro",
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
        "//ir/cmd:basic",
        "//ir/cmd:call",
        "//ir/cmd:cast",
        "//ir/cmd:jump",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:print",
        "//ir/cmd:register",
        "//ir/cmd:return",
        "//ir/cmd:scope",
        "//ir/cmd:store",
        "//ir/cmd:types",
        "//ir/cmd:util",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
    test_deps = None,
)

cc_library(
    name = "new_inliner",
    hdrs = ["new_inliner.h"],
    srcs = ["new_inliner.cc"],
    deps = [
        ":reg",
        ":reg_or",
        "//base:debug",
        "//ir/cmd:jump",
        "@com_google_absl//absl/container:flat_hash_map",
    ]
)

cc_test(
    name = "new_inliner_test",
    srcs = ["new_inliner_test.cc"],
    deps = [
        ":new_inliner",
        ":basic_block",
        "//test",
    ],
)

cc_library(
    name = "instructions",
    hdrs = ["instructions.h"],
    deps = [
        ":block_def",
        ":reg_or",
        ":scope_def",
        ":struct_field",
        ":new_inliner",
        "//ast/scope",
        "//base:clone",
        "//ir/cmd:basic",
        "//ir/cmd:call",
        "//ir/cmd:cast",
        "//ir/cmd:jump",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:print",
        "//ir/cmd:register",
        "//ir/cmd:return",
        "//ir/cmd:scope",
        "//ir/cmd:store",
        "//ir/cmd:types",

        "//type",
        "//type:pointer",
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
        "//ir/cmd:misc",
        "//ir/cmd:return",
    ],
    test_deps = None,
)

cc_lib(
    name = "components",
    deps = [
        ":builder",
        ":compiled_fn",
        "//ir/cmd:basic",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:types",
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
        ":builder",
        "//ast:ast",
        "//base:bag",
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
        ":jump",
        ":local_block_interpretation",
        ":new_inliner",
        ":reg",
        ":results",
        ":stack_frame_allocations",
        "//base:macros",
        "//ir/cmd:basic",
        "//ir/cmd:call",
        "//ir/cmd:cast",
        "//ir/cmd:jump",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:print",
        "//ir/cmd:register",
        "//ir/cmd:return",
        "//ir/cmd:scope",
        "//ir/cmd:store",
        "//ir/cmd:types",
        "//ir/cmd:cmd_xmacro",
        "//type:function",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "out_params",
    deps = [":reg"],
    test_deps = None,
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

