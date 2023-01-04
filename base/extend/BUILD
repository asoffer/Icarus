package(default_visibility = ["//visibility:public"])

cc_library(
    name = "absl_format",
    hdrs = ["absl_format.h"],
    deps = [
        "//base:extend",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "absl_hash",
    hdrs = ["absl_hash.h"],
    deps = [
        ":equality",
        "//base:extend",
    ],
)

cc_library(
    name = "compare",
    hdrs = ["compare.h"],
    deps = [
        ":equality",
        "//base:extend",
    ],
)

cc_library(
    name = "equality",
    hdrs = ["equality.h"],
    deps = ["//base:extend"],
)
