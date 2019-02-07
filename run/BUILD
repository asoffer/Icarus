package(default_visibility = ["//visibility:public"])

cc_library(
    name = "compiler",
    srcs = ["compiler.cc"],
    deps = [
        "//backend:exec",
        "//base/container:vector",
        "//base:untyped_buffer",
        "//base:util",
        "//misc:context",
        "//misc:module",
        "//ir:func",
    ],
)

cc_library(
    name = "repl",
    srcs = ["repl.cc"],
    deps = [
        "//ast:declaration",
        "//ast:expression",
        "//ast:fn_params",
        "//backend:exec",
        "//base/container:vector",
        "//base:untyped_buffer",
        "//frontend:source",
        "//ir:func",
        "//ir:cmd",
        "//misc:context",
        "//misc:module",
        "//type:function",
        "//type:tuple",
    ],
)
