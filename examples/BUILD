load("//toolchain:ic_rule.bzl", "ic_binary")

filegroup(
    name = "example_file",
    srcs = ["example_file.txt"],
)

ic_binary(
    name = "arguments",
    srcs = ["arguments.ic"],
    deps = ["//toolchain/stdlib:io"],
)

# ic_binary(
#     name = "c_file",
#     srcs = ["c_file.ic"],
#     deps = [
#         "//toolchain/stdlib/compat/c:errno",
#         "//toolchain/stdlib/compat/c:stdio",
#         "//toolchain/stdlib:io",
#     ],
#     data = [
#         ":example_file",
#     ],
# )

ic_binary(
    name = "date_time",
    srcs = ["date_time.ic"],
    deps = [
        "//toolchain/stdlib:io",
        "//toolchain/stdlib/compat/c:time",
        "//toolchain/stdlib/compat/c:string",
    ],
)
# 
# ic_binary(
#     name = "enum",
#     srcs = ["enum.ic"],
#     deps = ["//toolchain/stdlib:io"],
# )

ic_binary(
    name = "fizzbuzz",
    srcs = ["fizzbuzz.ic"],
    deps = ["//toolchain/stdlib:io"],
)

ic_binary(
    name = "foreign_function",
    srcs = ["foreign_function.ic"],
)

# ic_binary(
#     name = "function_calls",
#     srcs = ["function_calls.ic"],
#     deps = ["//toolchain/stdlib:io"],
# )
 
ic_binary(
    name = "import",
    srcs = ["import.ic"],
    deps = ["//toolchain/stdlib:io"],
)

ic_binary(
    name = "minimal_program",
    srcs = ["minimal_program.ic"],
)
