package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "compiler",
    deps = [
        "//base:untyped_buffer",
        "//base:util",
        "//compiler:compiler",
        "//compiler:executable_module",
        "//error:log",
        "//module:pending",
        "//interpretter:execute",
        "//ir:compiled_fn",
        "//module:module",
        "//opt",
    ],
    no_header = True,
    test_deps = None,
)

cc_lib(
    name = "repl",
    deps = [
        "//ast:ast",
        "//ast:expression",
        "//base:untyped_buffer",
        "//compiler:compiler",
        "//core:fn_params",
        "//frontend/source:repl",
        "//frontend:parse",
        "//interpretter:execute",
        "//ir:compiled_fn",
        "//module:module",
        "//type:function",
        "//type:tuple",
    ],
    no_header = True,
    test_deps = None,
)
