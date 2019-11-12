package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_binary(
    name = "icfmt",
    srcs = ["main.cc"],
    deps = [
        ":token_extractor",
        "//ast:ast",
        "//ast/scope:scope",
        "//ast/scope:exec",
        "//base:log",
        "//frontend:lex",
        "//frontend:parse",
        "//frontend/source:string",
        "//init:cli",
        "//init:signal",
        "//ir:results",
        "//ir:str",
        "//module:assign_scope",
        "//module:dependent_decls",
        "//module:module",
    ],
)

cc_lib(
    name = "token_extractor",
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
        "//base:debug",
    ],
    test_deps = None,
)
