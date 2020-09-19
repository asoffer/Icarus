package(default_visibility = ["//visibility:public"])

cc_library(
    name = "alignment",
    hdrs = ["alignment.h"],
    deps = [],
)
cc_library(
    name = "bytes",
    hdrs = ["bytes.h"],
    deps = [],
)

cc_library(
    name = "arch",
    hdrs = ["arch.h"],
    deps = [
        ":alignment",
        ":bytes",
        ":type_contour",
    ],
)

cc_test(
    name = "arch_test",
    srcs = ["arch_test.cc"],
    deps = [
        ":arch",
        "@com_google_googletest//:gtest_main",
    ]
)

cc_library(
    name = "call",
    hdrs = ["call.h"],
    deps = [
        ":dependency_node",
        ":fn_args",
        ":params",
        ":params_ref",
        "//base:log",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "call_test",
    srcs = ["call_test.cc"],
    deps = [
        ":call",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "dependency_node",
    hdrs = ["dependency_node.h"],
    deps = []
)

# TODO dependency_node_test

cc_library(
    name = "fn_args",
    hdrs = ["fn_args.h"],
    deps = [
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "fn_args_test",
    srcs = ["fn_args_test.cc"],
    deps = [
        ":fn_args",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "params",
    hdrs = ["params.h"],
    deps = [
        ":fn_args",
        "//base:debug",
        "//base:lazy_convert",
        "//base:macros",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "params_test",
    srcs = ["params_test.cc"],
    deps = [
        ":params",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "params_ref",
    hdrs = ["params_ref.h"],
    deps = [
        ":params",
        ":fn_args",
    ],
)

cc_test(
    name = "params_ref_test",
    srcs = ["params_ref_test.cc"],
    deps = [
        ":params_ref",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "ordered_fn_args",
    hdrs = ["ordered_fn_args.h"],
    deps = [
        ":fn_args",
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "ordered_fn_args_test",
    srcs = ["ordered_fn_args_test.cc"],
    deps = [
        ":ordered_fn_args",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "type_contour",
    hdrs = ["type_contour.h"],
    deps = [
        ":alignment",
        ":bytes",
    ],
)

cc_test(
    name = "type_contour_test",
    srcs = ["type_contour_test.cc"],
    deps = [
        ":type_contour",
        "@com_google_googletest//:gtest_main",
    ],
)
