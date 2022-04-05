package(default_visibility = ["//visibility:public"])

cc_library(
    name = "alignment",
    hdrs = ["alignment.h"],
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
    name = "arguments",
    hdrs = ["arguments.h"],
    deps = [
        "//base:debug",
        "//base:meta",
        "//base:universal_print",
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
    name = "bytes",
    hdrs = ["bytes.h"],
    deps = [],
)

cc_library(
    name = "call",
    hdrs = ["call.h"],
    deps = [
        ":dependency_node",
        ":arguments",
        ":parameters",
        "//base:extend",
        "//base/extend:equality",
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
    name = "cycle_tracker",
    hdrs = ["cycle_tracker.h"],
    deps = [
        "//base:debug",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/functional:function_ref",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "cycle_tracker_test",
    srcs = ["cycle_tracker_test.cc"],
    deps = [
        ":cycle_tracker",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "dependency_node",
    hdrs = ["dependency_node.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
    ]
)

cc_test(
    name = "dependency_node_test",
    srcs = ["dependency_node_test.cc"],
    deps = [
        ":dependency_node",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "lexeme",
    hdrs = ["lexeme.h"],
    deps = ["//base:debug"],
)

cc_library(
    name = "parameters",
    hdrs = ["parameters.h"],
    deps = [
        "//base:debug",
        "//base:extend",
        "//base/extend:absl_hash",
        "//base/extend:serialize",
        "//base:macros",
    ],
)

cc_test(
    name = "parameters_test",
    srcs = ["parameters_test.cc"],
    deps = [
        ":parameters",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "parser",
    hdrs = ["parser.h"],
    deps = [
        ":lexeme",
        "//base:compile_time_string",
        "//base:meta",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "parser_test",
    srcs = ["parser_test.cc"],
    deps = [
        ":parser",
        "@com_google_googletest//:gtest_main",
    ]
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
