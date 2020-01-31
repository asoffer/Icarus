package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "inference_failure_reason",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "log",
    textual_hdrs = ["errors.xmacro.h"],
    deps = [
        ":inference_failure_reason",
        "//ast:ast",
        "//core:fn_args",
        "//base:debug",
        "//diagnostic:console_renderer",
        "//diagnostic/consumer",
        "//diagnostic:message",
        "//frontend/source:range",
        "//frontend/source:source",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None,
)
