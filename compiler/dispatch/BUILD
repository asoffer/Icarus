package(default_visibility = ["//visibility:public"]) # TODO compiler:__subpackages__"])
load("//:defs.bzl", "cc_lib_target")

cc_lib_target(
    name = "dispatch",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":match",
        "//compiler:verify_result",
        "//core:fn_params",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":extract_params",
        "//ast:expression",
        "//ast:overload_set",
        "//ast/methods:dump",
        "//base:debug",
        "//base:expected",
        "//type:cast",
        "//type:variant",
    ],
    test_deps = None,
)

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

cc_lib_target(
    name = "match",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:ast_fwd",
        "//base:expected",
        "//compiler:verify_result",
        "//core:fn_args",
        "//core:fn_params",
        "//type:typed_value",
    ],
    impl_deps = [
        "//ast:ast",
        "//type:cast",
        "//type:type",
    ],
    test_deps = [
        ":extract_params-impl",
        "//test:module",
        "//type:variant",
        "//type:function",
    ],
)
