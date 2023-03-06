package(default_visibility = ["//visibility:public"])

cc_library(
    name = "bazel",
    visibility = ["//visibility:public"],
    hdrs = ["bazel.h"],
    srcs = ["bazel.cc"],
    deps = [
        "//module:module_name",
        "//base:debug",
        "//base:file",
        "//serialization:unique_module_id",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/functional:any_invocable",
        "@com_google_absl//absl/container:flat_hash_map",
    ]
)

cc_binary(
    name = "compile",
    srcs = ["compile.cc"],
    deps = [
        ":bazel",
        ":flags",
        "//base:file",
        "//base:log",
        "//frontend:parse",
        "//module",
        "//module:resources",
        "//semantic_analysis:context",
        "//semantic_analysis/type_verification:verify",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "flags",
    hdrs = ["flags.h"],
    srcs = ["flags.cc"],
    deps = [
        "//diagnostic/consumer",
        "//diagnostic/consumer:json",
        "//diagnostic/consumer:streaming",
        "//frontend:source_indexer",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:usage",
        "@com_google_absl//absl/status:statusor",
    ],
)

cc_binary(
    name = "run_bytecode",
    srcs = ["run_bytecode.cc"],
    deps = [
        ":bazel",
        ":flags",
        "//module",
        "//module:resources",
        "//vm:execute",
        "@asoffer_jasmin//jasmin:execute",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
    ],
)
