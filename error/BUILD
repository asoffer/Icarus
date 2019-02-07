package(default_visibility = ["//visibility:public"])

cc_library(
    name = "inference_failure_reason",
    hdrs = ["inference_failure_reason.h"],
)

cc_library(
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
