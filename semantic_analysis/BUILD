package(default_visibility = ["//visibility:public"])

cc_library(
    name = "context",
    hdrs = ["context.h"],
    srcs = ["context.cc"],
    deps = [
        ":type_system",
        "//ast",
        "//module:symbol",
        "@asoffer_nth//nth/debug",
    ]
)

cc_library(
    name = "function_data",
    hdrs = ["function_data.h"],
    deps = [
        "//ast:declaration",
        "//vm:function",
        "@asoffer_nth//nth/container:flyweight_map",
    ],
)

cc_library(
    name = "type_system",
    hdrs = ["type_system.h"],
    srcs = ["type_system.cc"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
        "//core:alignment",
        "//core:arch",
        "//core:bytes",
        "//core:type_contour",
        "//core/type_system",
        "//core/type_system:finite_set",
        "//core/type_system:function",
        "//core/type_system:parameter",
        "//core/type_system:pointer",
        "//core/type_system:sized_integer",
        "//data_types:integer",
        "//data_types:addr",
        "//data_types:char",
        "//serialization:module_index",
        "//serialization:type_system",
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
    hdrs = [
        "phase.h",
        "task.h",
        "yield_result.h",
    ],
    deps = [
        "//ast:node",
        "//base:extend",
        "//base/extend:absl_hash",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/meta:type",
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
        "@asoffer_nth//nth/meta:type",
        "@com_google_googletest//:gtest_main",
    ],
)
