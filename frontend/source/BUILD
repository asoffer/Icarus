package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "source",
    deps = [
        "//base:cast",
        "//base:expected",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "cursor",
    deps = [
        ":range",
        "//base:debug",
    ],
    header_only = True,
    test_deps = None,
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
        "//test",
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
        "//test",
    ],
    data = [
        "testdata/empty_file.txt",
        "testdata/one_line_file.txt",
        "testdata/multi_line_file.txt",
    ],
)

cc_lib(
    name = "id",
    deps = [],
    test_deps = []
)

cc_lib(
    name = "repl",
    deps = [":source"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "string",
    deps = [":source"],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "range",
    deps = [
        "//base:debug",
        "//base:interval",
        "//base:strong_types",
    ],
    header_only = True,
    test_deps = None,
)
