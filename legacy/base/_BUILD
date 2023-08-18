package(default_visibility = ["//visibility:public"])

cc_library(
    name = "cast",
    hdrs = ["cast.h"],
    deps = [
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "file",
    hdrs = ["file.h"],
    srcs = ["file.cc"],
    deps = [
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/io:file",
        "@asoffer_nth//nth/io:file_path",
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
    name = "graph",
    hdrs = ["graph.h"],
    deps = [
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
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
    name = "visitable",
    hdrs = ["visitable.h"],
    deps = [
        "@asoffer_nth//nth/meta:sequence",
        "@asoffer_nth//nth/meta:type",
    ],
)
