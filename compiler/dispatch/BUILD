package(default_visibility = ["//visibility:public"]) # TODO compiler:__subpackages__"])

cc_library(
    name = "jump_table_header",
    hdrs = ["jump_table.h"],
    deps = [
        ":overload",
        "//ast:ast",
        "//base:expected",
        "//core:fn_args",
        "//ir:jump",
        "//type:qual_type",
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
        "//compiler:compiler_header",
        "//ir:inliner",
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
        "//core:fn_args",
        "//core:params",
        "//ir:builder",
        "//type:qual_type",
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
        ":runtime",
        "//base:debug",
        "//base:defer",
        "//compiler:compiler_header",
        "//ir:builder",
        "//type",
        "//type:cast",
        "//type:qual_type",
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
        "//core:fn_args",
        "//core:params",
        "//ir:results",
        "//type:qual_type",
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
        ":runtime",
        "//base:debug",
        "//compiler:compiler_header",
        "//ir:builder",
        "//ir:results",
        "//type",
        "//type:cast",
        "//type:qual_type",
        "//type:typed_value",
        "//type:variant",
    ],
)

cc_test(
    name = "fn_call_table_test",
    srcs = ["fn_call_table_test.cc"],
    deps = [
        ":fn_call_table",
        "//compiler:compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "overload",
    hdrs = ["overload.h"],
    deps = [
        "//ast:ast",
        "//type:type",
        "//type:typed_value",
        "//core:params",
    ]
)

cc_library(
    name = "parameters_and_arguments",
    hdrs = ["parameters_and_arguments.h"],
    srcs = ["parameters_and_arguments.cc"],
    deps = [
        "//ast:ast",
        "//compiler:compiler_header",
        "//interpretter:evaluate",
        "//ir:results",
        "//type:cast",
        "//type:type",
        "//type:function",
    ],
)

cc_test(
    name = "parameters_and_arguments_test",
    srcs = ["parameters_and_arguments_test.cc"],
    deps = [
        "//compiler:compiler",
        "//test:module",
        "//type:function",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "match",
    hdrs = ["match.h"],
    srcs = ["match.cc"],
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//base:expected",
        "//type:cast",
        "//core:fn_args",
        "//core:params",
        "//type:qual_type",
        "//type:type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "match_test",
    srcs = ["match_test.cc"],
    deps = [
        ":match",
        ":parameters_and_arguments",
        "//compiler:compiler",
        "//test:module",
        "//type:variant",
        "//type:function",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "runtime",
    hdrs = ["runtime.h"],
    srcs = ["runtime.cc"],
    deps = [
        "//base:meta",
        "//core:fn_args",
        "//core:params",
        "//ir:basic_block",
        "//ir:builder",
        "//ir:results",
        "//ir/value:reg_or",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)  
