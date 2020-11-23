package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "format",
    srcs = ["main.cc"],
    deps = [
        ":token_extractor",
        "//ast:ast",
        "//base:no_destructor",
        "//diagnostic/consumer:streaming",
        "//frontend:parse",
        "//frontend/source:file_name",
        "//frontend/source:string",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings",
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
