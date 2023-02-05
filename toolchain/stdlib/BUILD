package(default_visibility = ["//visibility:private"])

load("//toolchain:ic_rule.bzl", "ic_library")

ic_library(
    name = "io",
    srcs = ["io.ic"],
)

ic_library(
    name = "stdlib",
    visibility = ["//visibility:public"],
    deps = [
        ":io",
    ],
)
