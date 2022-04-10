package(default_visibility = ["//visibility:public"])

cc_library(
    name = "addr",
    hdrs = ["addr.h"],
)

cc_library(
    name = "block",
    hdrs = ["block.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
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
        ":foreign_fn",
        ":native_fn",
        "//core:parameters",
        "//type:function",
        "//base:extend",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "foreign_fn",
    hdrs = ["foreign_fn.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:equality",
        "//base:flyweight_map",
        "//type:function",
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
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
        "//base/extend:equality",
        "//base:global",
        "//compiler:work_resources",
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
        "//base:serialize",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/strings",
    ],
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
        "//ir:subroutine",
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
    name = "reg",
    hdrs = ["reg.h"],
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
        "//base:universal_print",
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
        "//base:universal_print",
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
        ":block",
        ":reg",
        "//ir:subroutine",
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
        "//base/extend:serialize",
        "//type:scope",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "scope_context",
    hdrs = ["scope_context.h"],
    deps = [
        ":block",
        ":reg",
        ":result_buffer",
        ":scope",
        "//base:any_invocable",
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
        "//base/extend:serialize",
        "//base:ptr_union",
        "//compiler:work_resources",
        "//ir:subroutine",
        "//type:block",
        "//type:generic",
        "//type:scope",
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
