package(default_visibility = ["//visibility:public"]) # TODO compiler:__subpackages__"])
load("//:defs.bzl", "cc_lib")

cc_library(
    name = "table_header",
    hdrs = ["table.h"],
    deps = [
        ":match",
        "//ast:expression",
        "//ast:overload_set",
        "//base:expected",
        "//compiler:verify_result",
        "//core:fn_args",
        "//core:fn_params",
        "//ir:builder",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "table",
    srcs = ["table.cc"],
    deps = [
        ":table_header",
        ":extract_params",
        ":match",
        "//base:debug",
        "//compiler:compiler_header",
        "//compiler:verify_result",
        "//ir:builder",
        "//type",
        "//type:cast",
        "//type:variant",
    ],
)

cc_lib(
    name = "extract_params",
    deps = [
        "//ast:ast_fwd",
        "//ast:ast",
        "//backend:eval",
        "//compiler:compiler_header",
        "//type:type",
        "//type:function",
    ],
    test_deps = [
        "//backend:eval",
        "//compiler:compiler",
        "//test:module",
        "//type:function",
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
        "//compiler:compiler",
        "//test:module",
        "//type:variant",
        "//type:function",
    ],
)
