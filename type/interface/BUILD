package(default_visibility = ["//visibility:public"])

cc_library(
    name = "interface",
    hdrs = ["interface.h"],
    srcs = ["interface.cc"],
    deps = [
        "//base:global",
        "//core:params",
        "//type",
        "//type:cast",
        "@com_google_absl//absl/container:node_hash_set",
    ],
)

cc_test(
    name = "interface_test",
    srcs = ["interface_test.cc"],
    deps = [
        ":interface",
        "//type:pointer",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "ir",
    hdrs = ["ir.h"],
    deps = [
        ":interface",
        "//base:extend",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
    ],
)
