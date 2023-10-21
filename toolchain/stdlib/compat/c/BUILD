load("//toolchain:ic_rule.bzl", "ic_library")

package(default_visibility = ["//visibility:public"])

ic_library(
    name = "types",
    srcs = ["types.ic"],
)

ic_library(
    name = "ctype",
    srcs = ["ctype.ic"],
    deps = [":types"],
)

ic_library(
    name = "stdio",
    srcs = ["stdio.ic"],
    deps = [":types"],
)
