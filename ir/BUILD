package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_component", "cc_lib")

cc_lib(
    name = "addr",
    deps = [
        "//core:bytes",
        "//base:debug",
    ],
    test_deps = None,
)

cc_lib(
    name = "any_func",
    deps = [
        ":foreign",
        "//base:debug",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "basic_block",
    deps = [
        ":cmd_buffer",
        ":out_params",
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

cc_lib(
    name = "jump_handler",
    deps = [":block_group"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "builder",
    deps = [
        ":addr",
        ":basic_block",
        ":block_group",
        ":cmd_buffer",
        ":reg",
        "//base:debug",
        "//base:scope",
        "//base:tag",
        "//type:typed_value",
    ],
    test_deps = None,
)
   
# TODO now that ':builtin'; doesn't need any IR, move it up to ast and rename
# this to ':builtin'
cc_lib(
    name = "builtin_ir",
    deps = [
        ":any_func",
        ":compiled_fn",
        "//ir/cmd:jumps",
        "//ir/cmd:misc",
        "//ir/cmd:return",
    ],
    test_deps = None,
)

cc_component(
    name = "cmd_buffer",
    intf_deps = [
        "//base:untyped_buffer",
        "//core:arch",
    ],
    impl_deps = [
        ":compiled_fn",
        "//backend:exec",
        "//ir/cmd:basic",
        "//ir/cmd:call",
        "//ir/cmd:cast",
        "//ir/cmd:jumps",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:print",
        "//ir/cmd:register",
        "//ir/cmd:return",
        "//ir/cmd:scope",
        "//ir/cmd:store",
        "//ir/cmd:types",
    ],
    test_deps = None,
)

cc_component(
    name = "components",
    intf_deps = [
        ":builder",
        ":compiled_fn",
        "//ir/cmd:basic",
        "//ir/cmd:jumps",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:types",
    ],
    impl_deps = [
        ":builder",
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
    name = "foreign",
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
        ":builder",
        ":compiled_fn",
        ":reg",
        ":results",
        ":stack_frame_allocations",
        "//ir/cmd:jumps",
        "//ir/cmd:register",
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
