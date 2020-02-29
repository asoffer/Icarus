package(default_visibility = ["//visibility:public"])

cc_library(
    name = "source",
    hdrs = ["source.h"],
    srcs = ["source.cc"],
    deps = [
        "//base:cast",
        "//base:expected",
    ],
)

cc_library(
    name = "cursor",
    hdrs = ["cursor.h"],
    deps = [
        ":range",
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
        "//base:expected",
        "//base:strong_types",
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
    linkopts = ["-lstdc++fs"],
    deps = [
        "//base:expected",
        "//base:strong_types",
        "@com_google_absl//absl/strings:str_format",
    ],
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
    name = "repl",
    hdrs = ["repl.h"],
    deps = [":source"],
)

cc_library(
    name = "string",
    hdrs = ["string.h"],
    deps = [":source"],
)

cc_test(
    name = "string_test",
    srcs = ["string_test.cc"],
    deps = [
        ":string",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "range",
    hdrs = ["range.h"],
    deps = [
        "//base:debug",
        "//base:interval",
        "//base:strong_types",
    ],
)
