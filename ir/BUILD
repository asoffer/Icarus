package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_component", "cc_lib")

cc_group_target(
    name = "impl",
    deps = [
        ":basic_block-impl",
        ":block_group-impl",
        ":builder-impl",
        ":cmd_buffer-impl",
        ":compiled_fn-impl",
        ":components-impl",
        ":inliner-impl",
    ],
)

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

cc_component(
    name = "basic_block",
    intf_deps = [
        ":cmd_buffer",
        ":out_params",
        "//base:untyped_buffer",
    ],
    impl_deps = [
        ":builder",
        ":compiled_fn",
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

cc_component(
    name = "block_group",
    intf_deps = [
        ":basic_block",
        ":stack_frame_allocations",
        "//ast:ast_fwd",
        "//base:ptr_span",
        "//core:alignment",
        "//core:bytes",
        "//core:fn_params",
        "//type:type_fwd",
        "//type:typed_value",
    ],
    impl_deps = [
        ":inliner",
        "//type:pointer",
    ],
    test_deps = None,
)

cc_component(
    name = "jump_handler",
    intf_deps = [
        ":block_group",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "builder",
    intf_deps = [
        ":addr",
        ":basic_block",
        ":reg",
        "//base:debug",
        "//base:scope",
        "//base:tag",
        "//type:typed_value",
    ],
    impl_deps = [
        ":cmd_buffer",
        ":compiled_fn",
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
        ":inliner",
        "//base:untyped_buffer",
        "//core:arch",
    ],
    impl_deps = [
        ":compiled_fn",
        "//backend:exec",
        "//ir/cmd:basic",
        "//ir/cmd:call-impl",
        "//ir/cmd:cast",
        "//ir/cmd:jumps",
        "//ir/cmd:load",
        "//ir/cmd:misc-impl",
        "//ir/cmd:print-impl",
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

cc_component(
    name = "compiled_fn",
    intf_deps = [
        ":basic_block",
        ":block_group",
        ":builder",
        "//ast:ast",
        "//base:bag",
        "//base:move_func",
        "//base:ptr_span",
        "//base:scope",
        "//core:fn_params",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        "//ast:ast",
        "//core:arch",
        "//type:function",
        "//type:pointer",
    ],
    test_deps = None,
)

cc_component(
    name = "inliner",
    intf_deps = [
        ":reg",
        ":results",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":builder",
        ":compiled_fn",
        ":stack_frame_allocations",
        "//ir/cmd:jumps",
        "//ir/cmd:register",
        "//type:function",
        "//type:type",
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
