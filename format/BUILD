package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target")

cc_binary(
    name = "icfmt",
    srcs = ["main.cc"],
    deps = [
        ":token_extractor-impl-format",
        "//ast:ast-format",
        "//base:log-impl-format",
        "//core:impl-format",
        "//frontend:parse-impl-format",
        "//frontend:lex-impl-format",
        "//frontend/source:string-format",
        "//init:cli-impl-format",
        "//init:signal-impl-format",
        "//ir:results-impl-format",
        "//ir:str-impl-format",
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
