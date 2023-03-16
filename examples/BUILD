load("//toolchain:ic_rule.bzl", "ic_library", "ic_binary")

filegroup(
    name = "example_file",
    srcs = ["example_file.txt"],
)

ic_binary(
    name = "date_time",
    srcs = ["date_time.ic"],
    deps = [
        "//toolchain/stdlib:io",
        "//toolchain/stdlib/c:time",
        ":utility",
    ],
)

ic_binary(
    name = "enum",
    srcs = ["enum.ic"],
    deps = ["//toolchain/stdlib:io"],
)

ic_binary(
    name = "foreign_function",
    srcs = ["foreign_function.ic"],
)

ic_binary(
    name = "function_calls",
    srcs = ["function_calls.ic"],
    deps = ["//toolchain/stdlib:io"],
)

ic_binary(
    name = "import",
    srcs = ["import.ic"],
    deps = ["//toolchain/stdlib:io"],
)

ic_binary(
    name = "minimal_program",
    srcs = ["minimal_program.ic"],
)

ic_library(
    name = "utility",
    srcs = ["utility.ic"],
    deps = [
        "//toolchain/stdlib/c:string",
    ],
)
