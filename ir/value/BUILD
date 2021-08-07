package(default_visibility = ["//visibility:public"])

cc_library(
    name = "addr",
    hdrs = ["addr.h"],
)

cc_library(
    name = "arguments",
    hdrs = ["arguments.h"],
    deps = [
        ":result_buffer",
        "//core:arguments",
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
        "//base:extend",
        "//base/extend:equality",
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
    name = "char",
    hdrs = ["char.h"],
)

cc_library(
    name = "fn",
    hdrs = ["fn.h"],
    deps = [
        ":builtin_fn",
        ":foreign_fn",
        ":native_fn",
        "//core:params",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "//base:extend",
        "//base/extend:absl_hash",
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
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:equality",
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
        ":result_buffer",
        "//base:any_invocable",
        "//base:extend",
        "//base/extend:absl_hash",
        "//base/extend:equality",
        "//base:global",
        "//core:arguments",
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
        ":result_buffer",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "hashtag",
    hdrs = ["hashtag.h"],
)

cc_library(
    name = "integer",
    hdrs = ["integer.h"],
    deps = [
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "jump",
    hdrs = ["jump.h"],
)

cc_library(
    name = "label",
    hdrs = ["label.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "module_id",
    hdrs = ["module_id.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "native_fn",
    hdrs = ["native_fn.h"],
    srcs = ["native_fn.cc"],
    deps = [
        "//base:debug",
        "//base:extend",
        "//ir:compiled_fn",
        "//type:function",
        "@com_google_absl//absl/strings:str_format",
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
    srcs = ["overload_set.cc"],
    deps = [
        "//core:call",
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
        "//base:extend",
        "//base/extend:absl_hash",
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
    name = "result_buffer",
    hdrs = ["result_buffer.h"],
    srcs = ["result_buffer.cc"],
    deps = [
        ":addr",
        ":reg_or",
        "//base:meta",
        "//base:serialize",
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
    ]
)

cc_test(
    name = "result_buffer_test",
    srcs = ["result_buffer_test.cc"],
    deps = [
        ":result_buffer",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "scope",
    hdrs = ["scope.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
    ],
)

#TODO: scope_test

cc_library(
    name = "slice",
    hdrs = ["slice.h"],
    deps = [
        ":addr",
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
        "//base/extend:compare",
    ],
)

#TODO: slice_test

cc_library(
    name = "string",
    hdrs = ["string.h"],
    srcs = ["string.cc"],
    deps = [
        ":addr",
        ":char",
        ":slice",
        "//base:guarded",
        "//base:no_destructor",
        "//core:alignment",
        "//core:arch",
        "//core:bytes",
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
