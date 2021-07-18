package(default_visibility = ["//visibility:public"])

cc_library(
    name = "interface",
    hdrs = ["interface.h"],
    srcs = ["interface.cc"],
    deps = [
        "//base:global",
        "//core:arguments",
        "//core:call",
        "//core:params",
        "//type",
        "//type:callable",
        "//type:function",
        "//type:cast",
        "@com_google_absl//absl/container:node_hash_set",
    ],
)

cc_test(
    name = "interface_test",
    srcs = ["interface_test.cc"],
    deps = [
        ":interface",
        "//type:function",
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
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
    ],
)
