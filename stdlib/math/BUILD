load("//toolchain:ic_rule.bzl", "ic_low_level_library")

ic_low_level_library(
    name = "complex",
    visibility = ["//visibility:public"],
    srcs = ["complex.ic"],
    deps = [":core"],
    precompile = False,
)

ic_low_level_library(
    name = "constants",
    visibility = ["//visibility:public"],
    srcs = ["constants.ic"],
)

ic_low_level_library(
    name = "core",
    visibility = ["//visibility:public"],
    srcs = ["core.ic"],
    precompile = False,
)

ic_low_level_library(
    name = "trig",
    visibility = ["//visibility:public"],
    srcs = ["trig.ic"],
    deps = [":constants"],
    precompile = False,
)

ic_low_level_library(
    name = "math",
    visibility = ["//visibility:public"],
    deps = [
        ":complex",
        ":constants",
        ":core",
        ":trig",
    ],
    precompile = False,
)
