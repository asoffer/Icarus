package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "inference_failure_reason",
    hdrs = ["inference_failure_reason.h"],
)

cc_lib(
    name = "log",
    hdrs = ["log.h"],
    textual_hdrs = ["errors.xmacro.h"],
    deps = [
        ":inference_failure_reason",
        "//ast:fn_args",
        "//base/container:unordered_map",
        "//base/container:vector",
        "//base:debug",
        "//frontend:text_span",
    ],
)

cc_lib(
    name = "log-impl",
    srcs = ["log.cc"],
    textual_hdrs = ["errors.xmacro.h"],
    deps = [
        ":log",
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
