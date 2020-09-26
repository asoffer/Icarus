package(default_visibility = ["//visibility:private"])

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        "//ast",
        "//base:ptr_span",
        "//compiler",
        "//ir:compiled_fn",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_binary(
    name = "repl",
    srcs = ["main.cc"],
    deps = [
        ":module",
        ":source",
        "//ast:ast",
        "//diagnostic/consumer:streaming",
        "//init:cli",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "source",
    hdrs = ["source.h"],
    deps = ["//frontend/source"],
)
