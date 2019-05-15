package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "sources")
sources()

cc_lib(
    name = "compiler",
    srcs = ["compiler.cc"],
    deps = [
        "//backend:exec",
        "//base:untyped_buffer",
        "//base:util",
        "//core:pending_module",
        "//ir:compiled_fn",
        "//misc:context",
        "//misc:module",
        "//opt:combine_blocks",
    ],
)

cc_lib(
    name = "repl",
    srcs = ["repl.cc"],
    deps = [
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
