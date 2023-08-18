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
        ":parameters",
        "//base:extend",
        "//base/extend:equality",
        "@com_google_absl//absl/container:flat_hash_set",
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
        "@asoffer_nth//nth/debug",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/functional:function_ref",
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
        "@com_google_absl//absl/strings:str_format",
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
    name = "arguments",
    hdrs = ["arguments.h"],
    deps = [
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/io:string_printer",
        "@asoffer_nth//nth/meta:sequence",
        "@asoffer_nth//nth/meta:type",
        "@asoffer_nth//nth/strings/format:universal",
        "@asoffer_nth//nth/strings:interpolate",
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
    name = "integer",
    hdrs = ["integer.h"],
    deps = [
        "//core:serialize",
        "//serialization:constants",
        "@asoffer_nth//nth/container:flyweight_set",
        "@com_google_absl//absl/numeric:int128",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_test(
    name = "integer_test",
    srcs = ["integer_test.cc"],
    deps = [
        ":integer",
        "@asoffer_nth//nth/test:main",
    ],
)

cc_library(
    name = "parameters",
    hdrs = ["parameters.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/io:string_printer",
        "@asoffer_nth//nth/strings/format:universal",
        "@asoffer_nth//nth/strings:interpolate",
        "@com_google_absl//absl/container:flat_hash_map",
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
    name = "serialize",
    hdrs = ["serialize.h"],
    deps = [
        "@asoffer_nth//nth/base:attributes",
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
