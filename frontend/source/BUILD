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
    name = "cursor",
    hdrs = ["cursor.h"],
    deps = [
        ":buffer",
        "//base:debug",
    ],
)

cc_library(
    name = "file",
    hdrs = ["file.h"],
    srcs = ["file.cc"],
    deps = [
        ":file_name",
        ":source",
        ":string",
        "//base:expected",
        "//base:strong_types",
        "//diagnostic:message",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_test(
    name = "file_test",
    srcs = ["file_test.cc"],
    deps = [
        ":file",
        "@com_google_googletest//:gtest_main",
    ],
    data = [
        "testdata/empty_file.txt",
        "testdata/one_line_file.txt",
        "testdata/multi_line_file.txt",
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
    name = "shared",
    hdrs = ["shared.h"],
    srcs = ["shared.cc"],
    deps = [
        ":source",
        "//base:no_destructor",
    ],
)

cc_library(
    name = "source",
    hdrs = ["source.h"],
    deps = [
        ":line",
        ":buffer",
        "//base:cast",
        "//base:expected",
    ],
)

cc_library(
    name = "string",
    hdrs = ["string.h"],
    deps = [
        ":buffer",
        ":source"
    ],
)

cc_test(
    name = "string_test",
    srcs = ["string_test.cc"],
    deps = [
        ":string",
        "@com_google_googletest//:gtest_main",
    ],
)
