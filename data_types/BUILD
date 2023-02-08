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
    name = "fn",
    visibility = ["//visibility:public"],
    hdrs = ["fn.h"],
    deps = [
        "//serialization:module_index",
        "//base:extend",
        "//base/extend:absl_hash",
    ],
)

cc_library(
    name = "hashtag",
    visibility = ["//visibility:public"],
    hdrs = ["hashtag.h"],
)

cc_library(
    name = "integer",
    visibility = ["//visibility:public"],
    hdrs = ["integer.h"],
    srcs = ["integer.cc"],
    deps = [
        "//serialization:constants",
        "//base:debug",
        "@asoffer_nth//nth/numeric:integer",
        "@asoffer_nth//nth/container:flyweight_set",
        "@asoffer_jasmin//jasmin:instruction",
    ],
)

cc_test(
    name = "integer_test",
    srcs = ["integer_test.cc"],
    deps = [
        ":integer",
        "@com_google_googletest//:gtest_main",
    ],
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
