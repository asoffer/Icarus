package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "run_bytecode",
    srcs = ["run_bytecode.cc"],
    deps = [
        ":flags",
        "//module",
        "@asoffer_jasmin//jasmin:execute",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
    ],
)

cc_binary(
    name = "compile",
    srcs = ["compile.cc"],
    deps = [
        ":flags",
        "//base:file",
        "//base:log",
        "//frontend:parse",
        "//module",
        "//semantic_analysis:compiler_state",
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
