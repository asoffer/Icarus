load("//toolchain:ic_rule.bzl", "ic_low_level_library")

ic_low_level_library(
    name = "core",
    visibility = ["//visibility:public"],
    srcs = ["core.ic"],
    precompile = False,
)

ic_low_level_library(
    name = "errno",
    visibility = ["//visibility:public"],
    srcs = ["errno.ic"],
    deps = ["//stdlib/c:types"],
)

ic_low_level_library(
    name = "io",
    visibility = ["//visibility:public"],
    srcs = ["io.ic"],
    deps = [],
    precompile = False,
)

ic_low_level_library(
    name = "file",
    visibility = ["//visibility:public"],
    srcs = ["file.ic"],
    deps = [
        ":errno",
        ":memory",
        "//stdlib/c:stdio",
    ],
    precompile = False,
)

ic_low_level_library(
    name = "memory",
    visibility = ["//visibility:public"],
    srcs = ["memory.ic"],
    deps = [
        ":core",
        "//stdlib/c:string",
        "//stdlib/c:types",
    ],
    precompile = False,
)

ic_low_level_library(
    name = "string",
    visibility = ["//visibility:public"],
    srcs = ["string.ic"],
    deps = [
        ":memory",
    ],
    precompile = False,
)

ic_low_level_library(
    name = "stdlib",
    visibility = ["//visibility:public"],
    deps = [
        ":core",
        ":errno",
        ":file",
        ":io",
        ":memory",
        ":string",
        "//stdlib/c:stdlib",
        "//stdlib/c:stdio",
        "//stdlib/c:string",
        "//stdlib/c:types",
        "//stdlib/math",
    ],
    precompile = False,
)
