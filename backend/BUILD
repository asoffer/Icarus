package(default_visibility = ["//visibility:public"])
load("//:common.bzl", "LLVM_LINKOPTS")

cc_library(
    name = "function",
    hdrs = ["function.h"],
    srcs = ["function.cc"],
    linkopts = LLVM_LINKOPTS,
    deps = [
        ":type",
        "//base:log",
        "//compiler:module",
        "//ir:compiled_fn",
        "//ir/instruction",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "type",
    hdrs = ["type.h"],
    srcs = ["type.cc"],
    linkopts = LLVM_LINKOPTS,
    deps = [
        "//base:meta",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "//type:struct",
        "//type:tuple",
        "//type:type",
        "//type:visitor",
    ],
)

cc_test(
    name = "type_test",
    srcs = ["type_test.cc"],
    linkopts = LLVM_LINKOPTS,
    deps = [
        ":type",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "//type:struct",
        "//type:tuple",
        "//type:type",
        "//type:visitor",
        "@com_google_googletest//:gtest_main",
    ],
)
