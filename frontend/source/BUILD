package(default_visibility = ["//visibility:public"])

cc_library(
    name = "indexer",
    hdrs = ["indexer.h"],
    srcs = ["indexer.cc"],
    deps = [
        "//base:debug",
        "//ir/value:module_id",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "indexer_test",
    srcs = ["indexer_test.cc"],
    deps = [
        ":indexer",
        "@com_google_googletest//:gtest_main",
    ],
)
