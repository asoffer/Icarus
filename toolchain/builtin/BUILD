load("//toolchain/builtin:rule.bzl", "ic_builtin_library")

package(default_visibility = ["//visibility:private"])

cc_binary(
    name = "construct",
    srcs = ["construct.cc"],
    deps = [
        ":module",
        "//ir:serialize",
        "@nth_cc//nth/commandline:main",
        "@nth_cc//nth/debug/log",
        "@nth_cc//nth/io:file_path",
        "@nth_cc//nth/io/writer",
        "@nth_cc//nth/io/writer:file",
        "@nth_cc//nth/io/writer:string",
        "@nth_cc//nth/io/serialize",
        "@nth_cc//nth/process:exit_code",
    ],
)

ic_builtin_library(
    name = "builtin",
    visibility = ["//visibility:public"],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        "//common:identifier",
        "//common:module_id",
        "//common:pattern",
        "//ir:function",
        "//ir:module",
        "//type",
    ],
)
