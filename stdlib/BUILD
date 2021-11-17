load("//toolchain:ic_rule.bzl", "ic_low_level_library")

ic_low_level_library(
    name = "c_types",
    visibility = ["//visibility:public"],
    srcs = ["c_types.ic"],
)

ic_low_level_library(
    name = "core",
    visibility = ["//visibility:public"],
    srcs = ["core.ic"],
)

ic_low_level_library(
    name = "cstdlib",
    visibility = ["//visibility:public"],
    srcs = ["cstdlib.ic"],
    deps = [":c_types"],
)

ic_low_level_library(
    name = "cstdio",
    visibility = ["//visibility:public"],
    srcs = ["cstdio.ic"],
    deps = [":c_types"],
)

ic_low_level_library(
    name = "cstring",
    visibility = ["//visibility:public"],
    srcs = ["cstring.ic"],
    deps = [":c_types"],
)

ic_low_level_library(
    name = "errno",
    visibility = ["//visibility:public"],
    srcs = ["errno.ic"],
    deps = [":c_types"],
)

ic_low_level_library(
    name = "io",
    visibility = ["//visibility:public"],
    srcs = ["io.ic"],
    deps = [":core"],
)

ic_low_level_library(
    name = "file",
    visibility = ["//visibility:public"],
    srcs = ["file.ic"],
    deps = [
        ":core",
        ":cstdio",
        ":errno",
        ":memory",
    ],
)

ic_low_level_library(
    name = "memory",
    visibility = ["//visibility:public"],
    srcs = ["memory.ic"],
    deps = [
        ":core",
        ":cstring",
    ],
)

ic_low_level_library(
    name = "string",
    visibility = ["//visibility:public"],
    srcs = ["string.ic"],
    deps = [
        ":core",
        ":memory",
    ],
)

ic_low_level_library(
    name = "stdlib",
    visibility = ["//visibility:public"],
    deps = [
        ":core",
        ":cstdlib",
        ":cstdio",
        ":errno",
        ":file",
        ":io",
        ":memory",
        ":string",
        "//stdlib/math",
    ],
)
