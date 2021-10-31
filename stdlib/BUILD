load('//toolchain:ic_rule.bzl', 'ic_library')

ic_library(
    name = "core",
    visibility = ["//visibility:public"],
    srcs = ["core.ic"],
    deps = [],
)

ic_library(
    name = "cstdlib",
    visibility = ["//visibility:public"],
    srcs = ["cstdlib.ic"],
    deps = [],
)

ic_library(
    name = "cstdio",
    visibility = ["//visibility:public"],
    srcs = ["cstdio.ic"],
    deps = [],
)

ic_library(
    name = "errno",
    visibility = ["//visibility:public"],
    srcs = ["errno.ic"],
    deps = [],
)

ic_library(
    name = "io",
    visibility = ["//visibility:public"],
    srcs = ["io.ic"],
    deps = [":core"],
)

ic_library(
    name = "memory",
    visibility = ["//visibility:public"],
    srcs = ["memory.ic"],
    deps = [":core"],
)

ic_library(
    name = "string",
    visibility = ["//visibility:public"],
    srcs = ["string.ic"],
    deps = [
        ":core",
        ":memory",
    ],
)
