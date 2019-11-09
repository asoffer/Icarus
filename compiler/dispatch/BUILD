package(default_visibility = ["//visibility:public"]) # TODO compiler:__subpackages__"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "dispatch",
    deps = [
        ":extract_params",
        ":match",
        "//ast:expression",
        "//ast:overload_set",
        "//ast/methods:dump",
        "//base:debug",
        "//base:expected",
        "//compiler:verify_result",
        "//core:fn_params",
        "//type:cast",
        "//type:variant",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "extract_params",
    deps = [
        "//ast:ast_fwd",
        "//ast:ast",
        "//backend:eval",
        "//compiler:compiler",
        "//type:type",
        "//type:function",
    ],
    test_deps = [
        "//test:module",
        "//type:function",
        "//backend:eval",
    ],
)

cc_lib(
    name = "match",
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//base:expected",
        "//compiler:verify_result",
        "//type:cast",
        "//core:fn_args",
        "//core:fn_params",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = [
        ":extract_params",
        "//test:module",
        "//type:variant",
        "//type:function",
    ],
)
