load('//toolchain:ic_rule.bzl', 'ic_library')

ic_library(
    name = "constants",
    visibility = ["//visibility:public"],
    srcs = ["constants.ic"],
    deps = [],
)

ic_library(
    name = "core",
    visibility = ["//visibility:public"],
    srcs = ["core.ic"],
    deps = [],
)

ic_library(
    name = "trig",
    visibility = ["//visibility:public"],
    srcs = ["trig.ic"],
    deps = [":constants"],
)
