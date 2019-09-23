package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "cc_component")

cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = [
        ":addr-impl",
        ":basic_block-impl",
        ":builder-impl",
        ":builtin_ir-impl",
        ":cmd_buffer-impl",
        ":compiled_fn-impl",
        ":components-impl",
        ":inliner-impl",
        ":out_params-impl",
        ":reg-impl",
        ":results-impl",
        ":str-impl",
    ],
)

cc_component(
    name = "addr",
    intf_deps = ["//core:bytes"],
    impl_deps = ["//base:debug"],
    test_deps = None,
)

cc_component(
    name = "any_func",
    intf_deps = [
        ":foreign",
        "//base:debug",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "basic_block",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":cmd_buffer",
        ":out_params",
        "//base:untyped_buffer",
    ],
    impl_deps = [
        ":compiled_fn",
        "//type:type",
    ],
    test_deps = None,
)

cc_component(
    name = "block",
    intf_deps = [
        ":any_func",
        "//base:debug",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_lib_target(
    name = "builder",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":basic_block",
        "//base:debug",
        "//base:scope",
    ],
    impl_deps = [
        ":cmd_buffer",
        ":compiled_fn",
    ],
    test_deps = None,
)
   
# TODO now that ':builtin'; doesn't need any IR, move it up to ast and rename
# this to ':builtin'
cc_lib_target(
    name = "builtin_ir",
    cfgs = ["match", "compile"],
    intf_deps = [],
    impl_deps = [
        ":any_func",
        ":compiled_fn",
        "//ir/cmd:jumps",
        "//ir/cmd:misc",
        "//ir/cmd:return",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "cmd_buffer",
    cfgs = ["match", "compile"],
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

cc_lib_target(
    name = "components",
    cfgs = ["match", "compile"],
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

cc_component(
    name = "values",
    intf_deps = ["//base:strong_types"],
    impl_deps = None,
    test_deps = [],
)

cc_component(
    name = "foreign",
    impl_deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "compiled_fn",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":basic_block",
        ":builder",
        ":stack_frame_allocations",
        "//ast:ast",
        "//base:bag",
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

cc_lib_target(
    name = "inliner",
    cfgs = ["match", "compile"],
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

cc_component(
    name = "out_params",
    intf_deps = [":reg"],
    impl_deps = [],
    test_deps = None,
)

cc_component(
    name = "reg",
    intf_deps = ["//base:debug"],
    impl_deps = ["@com_google_absl//absl/strings"],
    test_deps = [],
)

cc_component(
    name = "reg_or",
    intf_deps = [":reg"],
    impl_deps = None,
    test_deps = [":reg-impl"],
)

cc_component(
    name = "results",
    intf_deps = [
        ":reg",
        ":reg_or",
        "//base:untyped_buffer",
        "//core:bytes",
    ],
    impl_deps = [
        ":reg-impl",
        "//base:untyped_buffer-impl",
        "@com_google_absl//absl/strings",
    ],
    test_deps = [],
)

cc_component(
    name = "scope_def",
    intf_deps = [
        ":any_func",
        "//base:move_func",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "stack_frame_allocations",
    intf_deps = [
        ":addr",
        ":reg",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "str",
    intf_deps = [":addr"],
    impl_deps = [
        "//base:guarded",
        "//base:untyped_buffer",
        "@com_google_absl//absl/container:node_hash_map",
    ],
    test_deps = None,
)
