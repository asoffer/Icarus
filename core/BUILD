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
        ":arguments",
        ":params",
        "//base:lazy",
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
    name = "arguments",
    hdrs = ["arguments.h"],
    deps = [
        "//base:meta",
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "arguments_test",
    srcs = ["arguments_test.cc"],
    deps = [
        ":arguments",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "params",
    hdrs = ["params.h"],
    deps = [
        ":arguments",
        "//base:debug",
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
    name = "ordered_arguments",
    hdrs = ["ordered_arguments.h"],
    deps = [
        ":arguments",
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "ordered_arguments_test",
    srcs = ["ordered_arguments_test.cc"],
    deps = [
        ":ordered_arguments",
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
