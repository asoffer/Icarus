load("//toolchain:ic_rule.bzl", "ic_library")

package(default_visibility = ["//visibility:public"])

ic_library(
    name = "io",
    srcs = ["io.ic"],
    deps = [
        "//toolchain/stdlib/compat/c:types",
        "//toolchain/stdlib/compat/c:stdio",
    ]
)
