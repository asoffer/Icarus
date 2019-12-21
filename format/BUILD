package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_binary(
    name = "format",
    srcs = ["main.cc"],
    deps = [
        ":token_extractor",
        "//ast:ast",
        "//frontend:parse",
        "//frontend/source:file_name",
        "//frontend/source:string",
        "//init:cli",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
    ],
)

cc_lib(
    name = "token_extractor",
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
        "//base:debug",
    ],
    test_deps = None,
)
