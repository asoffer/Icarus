package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")
sources()

cc_lib_target(
    name = "compiler",
    cfgs = ["compile"],
    intf_deps = None,
    impl_deps = [
        "//backend:exec",
        "//base:untyped_buffer",
        "//base:util",
        "//core:pending_module",
        "//ir:compiled_fn",
        "//misc:context",
        "//misc:module",
        "//opt:combine_blocks",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "repl",
    cfgs = ["compile"],
    intf_deps = None,
    impl_deps = [
        "//ast:declaration",
        "//ast:expression",
        "//backend:exec",
        "//base:untyped_buffer",
        "//core:fn_params",
        "//frontend:source",
        "//ir:compiled_fn",
        "//ir:cmd",
        "//misc:context",
        "//misc:module",
        "//type:function",
        "//type:tuple",
    ],
)
