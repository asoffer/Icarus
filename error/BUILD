package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "cc_component")

cc_component(
    name = "inference_failure_reason",
    impl_deps = None,
    test_deps = None)

cc_lib_target(
    name = "log",
    textual_hdrs = ["errors.xmacro.h"],
    intf_deps = [
        ":inference_failure_reason",
        "//ast:ast_fwd",
        "//core:fn_args",
        "//base:debug",
        "//diagnostic:console_renderer",
        "//frontend/source:range",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//ast:ast",
        "//diagnostic:diagnostic",
        "//diagnostic:console_renderer-impl",
        "//frontend/source:source",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None)
