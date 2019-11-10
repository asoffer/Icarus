package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component")

cc_component(
    name = "compiler",
    intf_deps = None,
    impl_deps = [
        "//backend:exec",
        "//base:untyped_buffer",
        "//base:util",
        "//compiler:compiler",
        "//compiler:module",
        "//error:log",
        "//module:pending",
        "//ir:compiled_fn",
        "//module:module",
        "//opt:combine_blocks",
    ],
    test_deps = None,
)

cc_component(
    name = "repl",
    intf_deps = None,
    impl_deps = [
        "//ast:ast",
        "//ast:expression",
        "//backend:exec",
        "//base:untyped_buffer",
        "//compiler:compiler",
        "//compiler:verify_type",
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
