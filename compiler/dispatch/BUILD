package(default_visibility = ["//visibility:public"]) # TODO compiler:__subpackages__"])
load("//:defs.bzl", "cc_component")

cc_component(
    name = "dispatch",
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

cc_component(
    name = "extract_params",
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
        "//type:function",
        "//backend:eval-impl",
    ],
)

cc_component(
    name = "match",
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
