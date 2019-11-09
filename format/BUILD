package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component")

cc_binary(
    name = "icfmt",
    srcs = ["main.cc"],
    deps = [
        ":token_extractor-impl",
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

cc_component(
    name = "token_extractor",
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:visitor",
        "//base:debug",
    ],
    impl_deps = [
        "//ast:ast",
    ],
    test_deps = None,
)
