package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")
sources()

cc_lib_target(
    name = "eval",
    cfgs = ["compile"],
    intf_deps = [
        ":exec",
        "//base:debug",
        "//base:untyped_buffer",
        "//misc:context",
    ],
    impl_deps = [
        "//core:arch",
        "//ast:expression",
        "//backend:exec",
        "//ir:compiled_fn",
        "//type:generic_struct",
        "//type:util",
        "//visitor:emit_ir",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "exec",
    cfgs = ["compile"],
    intf_deps = [
        "//base:untyped_buffer",
        "//ir:basic_block",
        "//ir:cmd",
    ],
    impl_deps = [
        ":eval",
        "//ast:block_literal",
        "//ast:expression",
        "//ast:function_literal",
        "//ast:scope_node",
        "//ast:struct_literal",
        "//base:util",
        "//error:log",
        "//ir:arguments",
        "//ir:compiled_fn",
        "//core:arch",
        "//misc:module",
        "//type:generic_struct",
        "//type:incomplete_enum",
        "//type:incomplete_flags",
        "//type:util",
    ],
    test_deps = None,
)
