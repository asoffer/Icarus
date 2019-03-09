package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "compiler",
    srcs = ["compiler.cc"],
    deps = [
        "//backend:exec",
        "//base:untyped_buffer",
        "//base:util",
        "//misc:context",
        "//misc:module",
        "//ir:func",
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
        "//ir:func",
        "//ir:cmd",
        "//misc:context",
        "//misc:module",
        "//type:function",
        "//type:tuple",
    ],
)
