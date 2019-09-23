package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target")

cc_binary(
    name = "icfmt",
    srcs = ["main.cc"],
    deps = [
        ":token_extractor-impl-format",
        "//ast:ast-format",
        "//base:log-impl",
        "//core:scope-impl-format",
        "//frontend:parse-impl-format",
        "//frontend:lex-impl-format",
        "//frontend/source:string",
        "//init:cli-impl",
        "//init:signal-impl",
        "//ir:results-impl",
        "//ir:str-impl",
        "//misc:module-impl-format",
        "//type:impl-format",
    ],
)

cc_lib_target(
    name = "token_extractor",
    cfgs = ["format"],
    intf_deps = [
        "//ast:ast_fwd",
        "//base:debug",
    ],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)
