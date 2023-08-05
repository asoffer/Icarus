package(default_visibility = ["//visibility:public"])
load("//toolchain:ic_rule.bzl", "ic_builtin")

cc_library(
    name = "bazel",
    visibility = ["//visibility:public"],
    hdrs = ["bazel.h"],
    srcs = ["bazel.cc"],
    deps = [
        "//module:module_name",
        "//module:unique_id",
        "//base:file",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/io:file_path",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/functional:any_invocable",
        "@com_google_absl//absl/container:flat_hash_map",
    ]
)

cc_binary(
    name = "builtin_compiler",
    srcs = ["builtin.cc"],
    deps = [
        "//core/type_system:type",
        "//module:global_function_map",
        "//module:type_system",
        "//serialization:module",
        "//semantic_analysis:type_system",
        "//vm:function",
        "//vm:serialization",
        "@asoffer_nth//nth/commandline:main",
        "@asoffer_nth//nth/io:file_path",
        "@asoffer_nth//nth/process:exit_code",
    ],
)

cc_binary(
    name = "compile",
    srcs = ["compile.cc"],
    deps = [
        ":bazel",
        "//base:file",
        "//diagnostic/consumer:streaming",
        "//frontend:parse",
        "//module",
        "//module:resources",
        "//semantic_analysis:context",
        "//semantic_analysis/type_verification:verify",
        "@asoffer_nth//nth/commandline:main",
        "@asoffer_nth//nth/io:file_path",
        "@asoffer_nth//nth/process:exit_code",
    ],
)

cc_binary(
    name = "run_bytecode",
    srcs = ["run_bytecode.cc"],
    deps = [
        ":bazel",
        "//diagnostic/consumer:streaming",
        "//frontend:source_indexer",
        "//module",
        "//module:resources",
        "//vm:execute",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_nth//nth/commandline:main",
        "@asoffer_nth//nth/io:file_path",
        "@asoffer_nth//nth/process:exit_code",
    ],
)

ic_builtin(
    name = "builtin"
)
