package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")
sources()

cc_lib_target(
    name = "eval",
    cfgs = ["match", "compile"],
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
        "//ir/cmd:jumps",
        "//ir/cmd:return",
        "//type:generic_struct",
        "//type:util",
        "//visitor:emit_ir",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "exec",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//base:untyped_buffer",
        "//ir:addr",
        "//ir:block",
        "//ir:compiled_fn",
        "//ir:reg",
        "//ir:reg_or",
    ],
    impl_deps = [
        ":eval",
        "//ast:ast",
        "//base:util",
        "//error:log",
        "//ir/cmd:jumps",
        "//ir:compiled_fn",
        "//core:arch",
        "//misc:module",
        "//type:generic_struct",
        "//type:util",
    ],
    test_deps = None,
)
