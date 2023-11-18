load("//toolchain:ic_rule.bzl", "ic_library")

package(default_visibility = ["//visibility:public"])

genrule(
    name = "generate_c_types",
    outs = ["types.ic"],
    cmd = "./$(location //toolchain/stdlib/compat/c/internal:c_types_generator) > $@",
    tools = ["//toolchain/stdlib/compat/c/internal:c_types_generator"],
    visibility = ["//visibility:private"],
)

ic_library(
    name = "types",
    srcs = [":generate_c_types"],
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

ic_library(
    name = "string",
    srcs = ["string.ic"],
    deps = [":types"],
)

ic_library(
    name = "time",
    srcs = ["time.ic"],
    deps = [":types"],
)
