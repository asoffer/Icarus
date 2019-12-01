package(default_visibility = ["//visibility:public"]) # TODO compiler:__subpackages__"])
load("//:defs.bzl", "cc_lib")

cc_library(
    name = "jump_table_header",
    hdrs = ["jump_table.h"],
    deps = [
        ":overload",
        "//ast:ast",
        "//base:expected",
        "//compiler:verify_result",
        "//core:fn_args",
        "//ir:jump",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "jump_table",
    srcs = ["jump_table.cc"],
    deps = [
        ":jump_table_header",
        ":match",
        ":parameters_and_arguments",
        "//base:debug",
    ],
)

cc_library(
    name = "scope_table_header",
    hdrs = ["scope_table.h"],
    deps = [
        ":jump_table_header",
        ":overload",
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
    name = "scope_table",
    srcs = ["scope_table.cc"],
    deps = [
        ":match",
        ":jump_table",
        ":parameters_and_arguments",
        ":scope_table_header",
        "//base:debug",
        "//compiler:compiler_header",
        "//compiler:extract_jumps",
        "//compiler:verify_result",
        "//ir:builder",
        "//type",
        "//type:cast",
        "//type:variant",
    ],
)

cc_library(
    name = "fn_call_table_header",
    hdrs = ["fn_call_table.h"],
    deps = [
        ":overload",
        "//ast:overload_set",
        "//base:expected",
        "//compiler:verify_result",
        "//core:fn_args",
        "//core:fn_params",
        "//ir:results",
        "//type:type",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "fn_call_table",
    srcs = ["fn_call_table.cc"],
    deps = [
        ":fn_call_table_header",
        ":match",
        ":parameters_and_arguments",
        "//base:debug",
        "//compiler:compiler_header",
        "//compiler:extract_jumps",
        "//compiler:verify_result",
        "//ir:builder",
        "//ir:results",
        "//type",
        "//type:cast",
        "//type:typed_value",
        "//type:variant",
    ],
)

cc_test(
    name = "fn_call_table-test",
    srcs = ["fn_call_table_test.cc"],
    deps = [
        ":fn_call_table",
        "//compiler:compiler",
        "//test",
        "//test:module",
    ],
)

cc_library(
    name = "overload",
    hdrs = ["overload.h"],
    deps = [
        "//ast:ast",
        "//type:type",
        "//type:typed_value",
        "//core:fn_params",
    ]
)

cc_lib(
    name = "parameters_and_arguments",
    deps = [
        "//ast:ast",
        "//backend:eval",
        "//compiler:compiler_header",
        "//type:cast",
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
        ":parameters_and_arguments",
        "//compiler:compiler",
        "//test:module",
        "//type:variant",
        "//type:function",
    ],
)
