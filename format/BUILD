package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "format",
    srcs = ["main.cc"],
    deps = [
        ":token_extractor",
        "//ast:ast",
        "//diagnostic/consumer:streaming",
        "//frontend:parse",
        "//frontend/source:file_name",
        "//frontend/source:string",
        "//init:cli",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
    ],
)

cc_library(
    name = "token_extractor",
    hdrs = ["token_extractor.h"],
    srcs = ["token_extractor.cc"],
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
        "//base:debug",
    ],
)
