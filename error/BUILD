package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
sources()

cc_lib(
    name = "inference_failure_reason",
    hdrs = ["inference_failure_reason.h"],
)

cc_target(
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
        "//ast:declaration",
        "//ast:identifier",
        "//base:interval",
        "//frontend:source",
    ],
)
