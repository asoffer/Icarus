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
        "@asoffer_nth//nth/meta:type",
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
    deps = [
        "@asoffer_nth//nth/meta:sequence",
        "@asoffer_nth//nth/meta:type",
    ],
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
    name = "global",
    hdrs = ["global.h"],
    deps = [
        ":guarded",
        "@asoffer_nth//nth/utility:no_destructor",
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
    name = "ptr_span",
    hdrs = ["ptr_span.h"],
    deps = [
        "@asoffer_nth//nth/meta:type",
    ],
)

cc_library(
    name = "ptr_union",
    hdrs = ["ptr_union.h"],
    deps = [
        ":debug",
        "@asoffer_nth//nth/meta:concepts",
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
        "@asoffer_nth//nth/meta:concepts",
        "@asoffer_nth//nth/meta:type",
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
    deps = [
        "@asoffer_nth//nth/meta:sequence",
        "@asoffer_nth//nth/meta:type",
    ],
)
