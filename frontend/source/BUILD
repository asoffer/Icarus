package(default_visibility = ["//visibility:public"])

cc_library(
    name = "buffer",
    hdrs = ["buffer.h"],
    srcs = ["buffer.cc"],
    deps = [
        ":line",
        "//base:debug",
        "//base:interval",
        "//base:strong_types",
        "@com_google_absl//absl/container:inlined_vector",
    ],
)

cc_test(
    name = "buffer_test",
    srcs = ["buffer_test.cc"],
    deps = [
        ":buffer",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "file",
    hdrs = ["file.h"],
    srcs = ["file.cc"],
    deps = [
        ":file_name",
        "//base:strong_types",
        "//diagnostic:message",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "file_name",
    hdrs = ["file_name.h"],
    srcs = ["file_name.cc"],
    deps = ["//base:strong_types"],
)

cc_test(
    name = "file_name_test",
    srcs = ["file_name_test.cc"],
    deps = [
        ":file_name",
        "@com_google_googletest//:gtest_main",
    ],
    data = [
        "testdata/empty_file.txt",
        "testdata/one_line_file.txt",
        "testdata/multi_line_file.txt",
    ],
)

cc_library(
    name = "line",
    hdrs = ["line.h"],
    deps = ["//base:strong_types"],
)

cc_library(
    name = "source",
    hdrs = ["source.h"],
    deps = [
        ":line",
        ":buffer",
        "//base:cast",
    ],
)

cc_library(
    name = "view",
    hdrs = ["view.h"],
    deps = [
        ":buffer",
        "//base:debug",
    ]
)

cc_library(
    name = "indexer",
    hdrs = ["indexer.h"],
    srcs = ["indexer.cc"],
    deps = [
        "//base:debug",
        "//ir/value:module_id",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "indexer_test",
    srcs = ["indexer_test.cc"],
    deps = [
        ":indexer",
        "@com_google_googletest//:gtest_main",
    ],
)
