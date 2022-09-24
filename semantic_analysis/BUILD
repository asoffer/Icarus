package(default_visibility = ["//visibility:public"])

cc_library(
    name = "type_system",
    hdrs = ["type_system.h"],
    srcs = ["type_system.cc"],
    deps = [
        "//core/type_system",
        "//core/type_system:finite_set",
        "//core/type_system:function",
        "//core/type_system:parameter",
        "//core/type_system:pointer",
        "//core/type_system:sized_integer",
    ]
)

cc_test(
    name = "type_system_test",
    srcs = ["type_system_test.cc"],
    deps = [
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

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
