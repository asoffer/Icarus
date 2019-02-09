package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_lib(
    name = "inference_failure_reason",
    hdrs = ["inference_failure_reason.h"],
)

cc_target(
    name = "log",
    textual_hdrs = ["errors.xmacro.h"],
    intf_deps = [
        ":inference_failure_reason",
        "//ast:fn_args",
        "//base:debug",
        "//frontend:text_span",
    ],
    impl_deps = [
        "//ast:declaration",
        "//ast:identifier",
        "//base:interval",
        "//misc:context",
        "//frontend:source",
        "//type:enum",
        "//type:opaque",
        "//type:struct",
        "//type:tuple",
        "//type:type",
        "//type:variant",
    ],
)
