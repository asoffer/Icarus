load("//toolchain:ic_rule.bzl", "ic_low_level_library")

ic_low_level_library(
    name = "constants",
    visibility = ["//visibility:public"],
    srcs = ["constants.ic"],
)

ic_low_level_library(
    name = "core",
    visibility = ["//visibility:public"],
    srcs = ["core.ic"],
)

ic_low_level_library(
    name = "trig",
    visibility = ["//visibility:public"],
    srcs = ["trig.ic"],
    deps = [":constants"],
)

ic_low_level_library(
    name = "math",
    visibility = ["//visibility:public"],
    deps = [
        ":constants",
        ":core",
        ":trig",
    ],
)
