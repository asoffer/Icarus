package(default_visibility = ["//module:__subpackages__"])

proto_library(
    name = "data_proto",
    srcs = ["data.proto"],
    deps = [],
)

cc_proto_library(
    name = "data_cc_proto",
    deps = [":data_proto"],
)

cc_library(
    name = "integer_table",
    visibility = ["//visibility:public"],
    hdrs = ["integer_table.h"],
    srcs = ["integer_table.cc"],
    deps = [
        ":data_cc_proto",
        "//base:debug",
        "@asoffer_nth//nth/numeric:integer",
        "@asoffer_nth//nth/container:flyweight_set",
    ],
)

cc_test(
    name = "integer_table_test",
    visibility = ["//visibility:public"],
    srcs = ["integer_table_test.cc"],
    deps = [
        ":integer_table",
        "@com_google_googletest//:gtest_main",
    ],
)
