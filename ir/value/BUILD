package(default_visibility = ["//visibility:public"])

cc_library(
    name = "addr",
    hdrs = ["addr.h"],
    srcs = ["addr.cc"],
    deps = [
        "//core:bytes",
        "//base:debug",
    ],
)

cc_test(
    name = "addr_test",
    srcs = ["addr_test.cc"],
    deps = [
        ":addr",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "block",
    hdrs = ["block.h"],
    deps = [],
)

#TODO: block_test

cc_library(
    name = "builtin_fn",
    hdrs = ["builtin_fn.h"],
    deps = [
        "//type:function",
        "//type:primitive",
    ],
)

cc_test(
    name = "builtin_fn_test",
    srcs = ["builtin_fn_test.cc"],
    deps = [
        ":builtin_fn",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "enum_and_flags",
    hdrs = ["enum_and_flags.h"],
    deps = [
        "//base:strong_types",
    ],
)

cc_test(
    name = "enum_and_flags_test",
    srcs = ["enum_and_flags_test.cc"],
    deps = [
        ":enum_and_flags",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "fn",
    hdrs = ["fn.h"],
    deps = [
        ":builtin_fn",
        ":foreign_fn",
        ":native_fn",
    ],
)

cc_test(
    name = "fn_test",
    srcs = ["fn_test.cc"],
    deps = [
        ":fn",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "foreign_fn",
    hdrs = ["foreign_fn.h"],
    srcs = ["foreign_fn.cc"],
    deps = [
        "//base:debug",
        "//base:flyweight_map",
        "//base:guarded",
        "//type:function",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "foreign_fn_test",
    srcs = ["foreign_fn_test.cc"],
    deps = [
        ":foreign_fn",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "generic_fn",
    hdrs = ["generic_fn.h"],
    srcs = ["generic_fn.cc"],
    deps = [
        ":native_fn",
        "//base:any_invocable",
        "//base:global",
        "//core:fn_args",
        "//type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "generic_fn_test",
    srcs = ["generic_fn_test.cc"],
    deps = [
        ":generic_fn",
        ":reg_or",
        ":value",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "hashtag",
    hdrs = ["hashtag.h"],
)

cc_library(
    name = "jump",
    hdrs = ["jump.h"],
)

cc_library(
    name = "label",
    hdrs = ["label.h"],
    deps = ["//base:strong_types"],
)

cc_library(
    name = "module_id",
    hdrs = ["module_id.h"],
    deps = [
        "//base:flyweight_map",
        "//base:global",
        "//frontend/source:file_name",
    ],
)

cc_library(
    name = "native_fn",
    hdrs = ["native_fn.h"],
    srcs = ["native_fn.cc"],
    deps = [
        "//base:debug",
        "//ir:compiled_fn",
        "//type:function",
    ],
)

cc_test(
    name = "native_fn_test",
    srcs = ["native_fn_test.cc"],
    deps = [
        ":native_fn",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "overload_set",
    hdrs = ["overload_set.h"],
    deps = [
        "//core:call",
        "//core:params_ref",
        "//ir/value:fn",
        "//type:cast",
        "//type:function",
        "//type:qual_type",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "overload_set_test",
    srcs = ["overload_set_test.cc"],
    deps = [
        ":overload_set",
        "//ir/value:fn",
        "//type:type",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ]
)

cc_library(
    name = "reg",
    hdrs = ["reg.h"],
    srcs = ["reg.cc"],
    deps = [
        "//base:debug",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "reg_test",
    srcs = ["reg_test.cc"],
    deps = [
        ":reg",
        "//base:stringify",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "reg_or",
    hdrs = ["reg_or.h"],
    deps = [":reg"],
)

cc_test(
    name = "reg_or_test",
    srcs = ["reg_or_test.cc"],
    deps = [
        ":reg_or",
        "//base:stringify",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "scope",
    hdrs = ["scope.h"],
    deps = [],
)

#TODO: scope_test

cc_library(
    name = "string",
    hdrs = ["string.h"],
    srcs = ["string.cc"],
    deps = [
        ":addr",
        "//base:no_destructor",
        "//ir:read_only_data",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)

cc_test(
    name = "string_test",
    srcs = ["string_test.cc"],
    deps = [
        ":string",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "value",
    hdrs = ["value.h"],
    deps = [
        ":addr",
        ":block",
        ":enum_and_flags",
        ":hashtag",
        ":jump",
        ":label",
        ":module_id",
        ":reg_or",
        ":scope",
        ":string",
        "//base:guarded",
        "//base:meta",
        "//core:fn_args",
        "//type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "value_test",
    srcs = ["value_test.cc"],
    deps = [
        ":reg_or",
        ":value",
        "@com_google_googletest//:gtest_main",
    ],
)
