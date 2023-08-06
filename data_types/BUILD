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
    name = "integer",
    visibility = ["//visibility:public"],
    hdrs = ["integer.h"],
    srcs = ["integer.cc"],
    deps = [
        "//serialization:constants",
        "@asoffer_nth//nth/container:flyweight_set",
        "@asoffer_nth//nth/debug",
        "@asoffer_jasmin//jasmin:instruction",
        "@com_google_absl//absl/numeric:int128",
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
