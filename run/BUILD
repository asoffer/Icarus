package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "compiler",
    deps = [
        "//base:defer",
        "//base:untyped_buffer",
        "//compiler:compiler",
        "//compiler:executable_module",
        "//frontend/source:file_name",
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
    no_header = True,
    test_deps = None,
)
