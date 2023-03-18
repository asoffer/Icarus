package(default_visibility = ["//visibility:private"])

load("//toolchain:ic_rule.bzl", "ic_library")

ic_library(
    name = "types",
    srcs = ["types.ic"],
    visibility = ["//visibility:public"],
)

ic_library(
    name = "ctype",
    srcs = ["ctype.ic"],
    visibility = ["//visibility:public"],
    deps= [":types"],
)

ic_library(
    name = "errno",
    srcs = ["errno.ic"],
    visibility = ["//visibility:public"],
    deps= [":types"],
)

ic_library(
    name = "math",
    srcs = ["math.ic"],
    visibility = ["//visibility:public"],
    deps= [":types"],
)

ic_library(
    name = "stdio",
    srcs = ["stdio.ic"],
    visibility = ["//visibility:public"],
    deps= [":types"],
)

ic_library(
    name = "stdlib",
    srcs = ["stdlib.ic"],
    visibility = ["//visibility:public"],
    deps= [":types"],
)

ic_library(
    name = "string",
    srcs = ["string.ic"],
    visibility = ["//visibility:public"],
    deps= [":types"],
)

ic_library(
    name = "time",
    srcs = ["time.ic"],
    visibility = ["//visibility:public"],
    deps= [":types"],
)
