package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "sources", "cc_lib", "cc_group_target", "cc_lib_target")
sources()

cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = [
        ":addr-impl",
        ":arguments-impl",
        ":basic_block-impl",
        ":block-impl",
        ":builtin_ir-impl",
        ":cmd_buffer-impl",
        ":compiled_fn-impl",
        ":components-impl",
        ":flags_val-impl",
        ":foreign-impl",
        ":inliner-impl",
        ":out_params-impl",
        ":results-impl",
        ":str-impl",
    ],
)

cc_lib_target(
    name = "addr",
    intf_deps = ["//core:bytes"],
    impl_deps = ["//base:debug"],
    test_deps = None,
)

cc_lib_target(
    name = "any_func",
    intf_deps = [
        ":foreign",
        "//base:debug",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "arguments",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":register",
        ":results",
        "//base:untyped_buffer",
        "//type:function",
        "//type:generic_struct",
        "//type:util",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//core:arch",
    ],
    test_deps = [
        ":addr-impl",
        ":results-impl",
        "//core:scope-impl",
        "//misc:module-impl",
        "//type:impl",
        "//:impl",
    ],
)

cc_lib_target(
    name = "basic_block",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":arguments",
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

cc_lib_target(
    name = "block",
    intf_deps = [
        ":any_func",
        "//base:debug",
    ],
    impl_deps = [],
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
        ":compiled_fn",
        ":register",
        "//ir/cmd:basic",
        "//ir/cmd:jumps",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:types",
    ],
    impl_deps = [
        "//misc:context",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "flags_val",
    intf_deps = ["//base:strong_types"],
    impl_deps = ["//type:flags"],
    test_deps = None,
)

cc_lib_target(
    name = "foreign",
    impl_deps = [
        "//base:guarded",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "compiled_fn",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":basic_block",
        ":stack_frame_allocations",
        "//ast:ast",
        "//base:bag",
        "//core:fn_params",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        ":arguments",
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
        ":register",
        ":results",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":compiled_fn",
        ":stack_frame_allocations",
        "//ir/cmd:jumps",
        "//ir/cmd:register",
        "//type:function",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "out_params",
    cfgs = ["match", "compile"],
    intf_deps = [":register"],
    impl_deps = [],
    test_deps = None,
)

cc_lib_target(
    name = "reg",
    intf_deps = [],
)

# TODO break up this target further.
cc_lib_target(
    name = "register",
    intf_deps = [
        ":reg",
        "//base:strong_types",
    ],
)

cc_lib_target(
    name = "results",
    intf_deps = [
        ":register",
        "//base:untyped_buffer",
        "//core:bytes",
    ],
    impl_deps = ["//base:untyped_buffer-impl"],
    test_deps = [],
)

cc_lib_target(
    name = "scope_def",
    intf_deps = [":any_func"],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "stack_frame_allocations",
    intf_deps = [
        ":addr",
        ":reg",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "str",
    intf_deps = [":addr"],
    impl_deps = [
        "//base:guarded",
        "//base:untyped_buffer",
        "@com_google_absl//absl/container:node_hash_map",
    ],
    test_deps = None,
)
