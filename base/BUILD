package(default_visibility = ["//visibility:public"])

cc_library(
    name = "cast",
    hdrs = ["cast.h"],
    deps = [":debug"],
)

cc_library(
    name = "debug",
    hdrs = ["debug.h"],
    deps = [
        ":log",
        ":universal_print",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "file",
    hdrs = ["file.h"],
    srcs = ["file.cc"],
    deps = [
        "@com_google_absl//absl/cleanup",
    ]
)

cc_library(
    name = "extend",
    hdrs = ["extend.h"],
    deps = [":meta"],
)

cc_test(
    name = "extend_test",
    srcs = ["extend_test.cc"],
    deps = [
        ":extend",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "flyweight_map",
    hdrs = ["flyweight_map.h"],
    deps = [
        ":debug",
        ":meta",
        "@com_google_absl//absl/container:flat_hash_set"
    ],
)

cc_test(
    name = "flyweight_map_test",
    srcs = ["flyweight_map_test.cc"],
    deps = [
        ":flyweight_map",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "flyweight_set",
    hdrs = ["flyweight_set.h"],
    deps = [
        ":debug",
        ":meta",
        "@com_google_absl//absl/container:flat_hash_set"
    ],
)

cc_test(
    name = "flyweight_set_test",
    srcs = ["flyweight_set_test.cc"],
    deps = [
        ":flyweight_set",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "global",
    hdrs = ["global.h"],
    deps = [
        ":guarded",
        ":no_destructor",
    ],
)

cc_library(
    name = "graph",
    hdrs = ["graph.h"],
    deps = [
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "guarded",
    hdrs = ["guarded.h"],
    deps = [
        "@com_google_absl//absl/synchronization",
    ],
)

cc_test(
    name = "guarded_test",
    srcs = ["guarded_test.cc"],
    deps = [
        ":guarded",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "interval",
    hdrs = ["interval.h"],
    deps = [":debug"],
)

cc_library(
    name = "iterator",
    hdrs = ["iterator.h"],
)

cc_test(
    name = "iterator_test",
    srcs = ["iterator_test.cc"],
    deps = [
        ":iterator",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "log",
    srcs = ["log.cc"],
    hdrs = ["log.h"],
    deps = [
        ":global",
        ":universal_print",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/synchronization",
    ],
)

cc_library(
    name = "macros",
    hdrs = ["macros.h"],
    deps = [],
)

cc_library(
    name = "meta",
    hdrs = ["meta.h"],
    deps = [
        "@asoffer_nth//nth/meta:type",
    ],
)

cc_test(
    name = "meta_test",
    srcs = ["meta_test.cc"],
    deps = [
        ":meta",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "no_destructor",
    hdrs = ["no_destructor.h"],
)

cc_test(
    name = "no_destructor_test",
    srcs = ["no_destructor_test.cc"],
    deps = [
        ":no_destructor",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "ptr_span",
    hdrs = ["ptr_span.h"],
    deps = [":meta"],
)

cc_library(
    name = "ptr_union",
    hdrs = ["ptr_union.h"],
    deps = [
        ":debug",
        ":meta",
    ],
)

cc_test(
    name = "ptr_union_test",
    srcs = ["ptr_union_test.cc"],
    deps = [
        ":ptr_union",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "universal_print",
    hdrs = ["universal_print.h"],
    deps = [
        ":meta",
        "@com_google_absl//absl/status:statusor",
    ],
)

cc_test(
    name = "universal_print_test",
    srcs = ["universal_print_test.cc"],
    deps = [
        ":universal_print",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "visitable",
    hdrs = ["visitable.h"],
    deps = [":meta"],
)
