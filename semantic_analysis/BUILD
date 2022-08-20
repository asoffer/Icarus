package(default_visibility = ["//visibility:public"])


cc_library(
    name = "task",
    hdrs = ["task.h"],
    deps = [
        "//ast:node",
        "//base:extend",
        "//base/extend:absl_hash",
        "//base:debug",
        "//base:meta",
        "@com_google_absl//absl/functional:any_invocable",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_test(
    name = "task_test",
    srcs = ["task_test.cc"],
    deps = [
        ":task",
        "@com_google_googletest//:gtest_main",
    ],
)
