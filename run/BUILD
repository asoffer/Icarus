package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target")

cc_lib_target(
    name = "compiler",
    cfgs = ["compile"],
    intf_deps = None,
    impl_deps = [
        "//backend:exec",
        "//base:untyped_buffer",
        "//base:util",
        "//error:log",
        "//module:pending",
        "//ir:compiled_fn",
        "//module:module",
        "//opt:combine_blocks",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "repl",
    cfgs = ["compile"],
    intf_deps = None,
    impl_deps = [
        "//ast:ast",
        "//ast:expression",
        "//backend:exec",
        "//base:untyped_buffer",
        "//core:fn_params",
        "//frontend/source:repl",
        "//frontend:parse",
        "//ir:compiled_fn",
        "//ir/cmd:jumps",
        "//module:module",
        "//type:function",
        "//type:tuple",
    ],
)
