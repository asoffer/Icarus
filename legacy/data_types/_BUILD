package(default_visibility = ["//visibility:private"])

cc_library(
    name = "addr",
    visibility = ["//visibility:public"],
    hdrs = ["addr.h"],
)

cc_library(
    name = "char",
    visibility = ["//visibility:public"],
    hdrs = ["char.h"],
    deps = [],
)

cc_library(
    name = "hashtag",
    visibility = ["//visibility:public"],
    hdrs = ["hashtag.h"],
)

cc_library(
    name = "label",
    visibility = ["//visibility:public"],
    hdrs = ["label.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
    ],
)
