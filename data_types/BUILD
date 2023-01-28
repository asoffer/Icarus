package(default_visibility = ["//visibility:private"])

proto_library(
    name = "data_proto",
    visibility = ["//module:__subpackages__"],
    srcs = ["data.proto"],
    deps = [],
)

cc_proto_library(
    name = "data_cc_proto",
    deps = [":data_proto"],
)

cc_library(
    name = "integer",
    visibility = ["//visibility:public"],
    hdrs = ["integer.h"],
    srcs = ["integer.cc"],
    deps = [
        ":data_cc_proto",
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
