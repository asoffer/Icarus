package(default_visibility = ["//visibility:public"])
load("//backend:common.bzl", "LLVM_LINKOPTS")

cc_library(
    name = "emit",
    hdrs = ["emit.h"],
    deps = [
        "//compiler:module",
        "//ir:subroutine",
        "//ir/value:reg",
        "//type",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "llvm",
    hdrs = ["llvm.h"],
    srcs = ["llvm.cc"],
    linkopts = LLVM_LINKOPTS,
    deps = [
        ":emit",
        ":type",
        "//base:log",
        "//compiler:module",
        "//ir:subroutine",
        "//ir/instruction",
        "//ir/instruction:arithmetic",
        "//ir/instruction:compare",
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
        "//type:slice",
        "//type:struct",
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
        "//type:type",
        "//type:visitor",
        "@com_google_googletest//:gtest_main",
    ],
)
