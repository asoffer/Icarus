package(default_visibility = ["//visibility:public"])

cc_library(
    name = "compiler",
    srcs = ["compiler.cc"],
    deps = [
        "//base:defer",
        "//base:untyped_buffer",
        "//compiler:compiler",
        "//compiler:executable_module",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//module:pending",
        "//interpretter:execute",
        "//ir:compiled_fn",
        "//module:module",
        "//opt",
    ],
)

cc_library(
    name = "repl",
    srcs = ["repl.cc"],
    deps = [
        "//ast:ast",
        "//ast:expression",
        "//base:untyped_buffer",
        "//compiler:compiler",
        "//core:params",
        "//diagnostic/consumer:streaming",
        "//frontend/source:repl",
        "//frontend:parse",
        "//interpretter:execute",
        "//ir:compiled_fn",
        "//module:module",
        "//type:function",
        "//type:tuple",
    ],
)
