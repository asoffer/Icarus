package(default_visibility = ["//compiler:__subpackages__"])
load("//:defs.bzl", "cc_lib_target", "cc_component", "icarus_method")

cc_lib_target(
    name = "extract_params",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:ast_fwd",
        "//compiler:compiler",
        "//type:type"
    ],
    impl_deps = [
        "//ast:ast",
        "//type:function",
        "//backend:eval",
    ],
    test_deps = [
        "//test:module",
        "//type:type-impl",
        "//type:function-impl",
        "//backend:eval-impl",
    ],
)
