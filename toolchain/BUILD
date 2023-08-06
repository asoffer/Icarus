package(default_visibility = ["//visibility:public"])
load("//toolchain:ic_rule.bzl", "ic_builtin")

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
        "//base:file",
        "//diagnostic/consumer:streaming",
        "//frontend:parse",
        "//module",
        "//module:module_map",
        "//module:resources",
        "//module:serialize",
        "//module:unique_id",
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
        "//diagnostic/consumer:streaming",
        "//frontend:source_indexer",
        "//module",
        "//module:module_map",
        "//module:resources",
        "//module:serialize",
        "//module:unique_id",
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
