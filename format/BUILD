package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "icarus_method")

icarus_method(
    name = "type_methods",
    deps = ["//module:extract_defining_modules-xmacro"],
    gen = False,
)

cc_lib_target(
    name = "type_dependencies",
    cfgs = ["format"],
    textual_hdrs = ["type_dependencies.h"],
    intf_deps = [
        "//format:type_methods-xmacro",
        "//module:extract_defining_modules"
    ],
)

cc_binary(
    name = "icfmt",
    srcs = ["main.cc"],
    deps = [
        ":token_extractor-impl-format",
        "//ast:ast-format",
        "//ast/scope:scope-impl-format",
        "//ast/scope:exec-impl-format",
        "//base:log-impl",
        "//frontend:lex-impl-format",
        "//frontend:parse-impl-format",
        "//frontend/source:string",
        "//init:cli-impl",
        "//init:signal-impl",
        "//ir:results-impl",
        "//ir:str-impl",
        "//module:assign_scope-impl-format",
        "//module:dependent_decls-impl-format",
        "//module:extract_defining_modules-impl-format",
        "//module:module-impl-format",
        "//type:impl-format",
    ],
)

cc_lib_target(
    name = "token_extractor",
    cfgs = ["format"],
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
