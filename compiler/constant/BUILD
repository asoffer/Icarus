package(default_visibility = ["//visibility:public"])

cc_library(
    name = "binding",
    hdrs = ["binding.h"],
    srcs = ["binding.cc"],
    deps = [
        "//ast:ast",
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//core:arch",
        "//core:bytes",
        "//ir:results",
        "//type:type_fwd",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_library(
    name = "binding_tree",
    hdrs = ["binding_tree.h"],
    deps = [":binding"],
)

cc_test(
    name = "binding_tree_test",
    srcs = ["binding_tree_test.cc"],
    deps = [
        ":binding_tree",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "binding_map",
    hdrs = ["binding_map.h"],
    deps = [
        ":binding_tree",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "binding_map_test",
    srcs = ["binding_map_test.cc"],
    deps = [
        ":binding_map",
        "@com_google_googletest//:gtest_main",
    ],
)
