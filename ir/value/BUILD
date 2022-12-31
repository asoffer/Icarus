package(default_visibility = ["//visibility:public"])

cc_library(
    name = "addr",
    hdrs = ["addr.h"],
)

cc_library(
    name = "char",
    hdrs = ["char.h"],
    deps = [
        "//base:serialize",
    ],
)

cc_library(
    name = "fn",
    hdrs = ["fn.h"],
    deps = [
        ":module_id",
        "//base:extend",
        "//base/extend:absl_hash",
        "//base/extend:serialize",
    ],
)

cc_library(
    name = "hashtag",
    hdrs = ["hashtag.h"],
)

cc_library(
    name = "integer",
    hdrs = ["integer.h"],
    deps = [
        "//base:serialize",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "label",
    hdrs = ["label.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "module_id",
    hdrs = ["module_id.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "serialized_pointer",
    hdrs = ["serialized_pointer.h"],
    deps = [],
)
