package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")
sources()

cc_lib_target(
    name = "inference_failure_reason",
    impl_deps = None,
    test_deps = None)

cc_lib_target(
    name = "log",
    textual_hdrs = ["errors.xmacro.h"],
    intf_deps = [
        ":inference_failure_reason",
        "//core:fn_args",
        "//base:debug",
        "//frontend:text_span",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//ast:ast",
        "//base:interval",
        "//frontend:source",
    ],
    test_deps = None)
