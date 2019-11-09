package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "source",
    deps = [
        "//base:cast",
        "//base:expected",
        "//base:util",
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

cc_lib(
    name = "file",
    deps = [
        ":source",
        "//base:expected",
        "@com_google_absl//absl/strings:str_format",
    ],
    test_deps = [],
    test_data = [
        "testdata/empty_file.txt",
        "testdata/one_line_file.txt",
        "testdata/multi_line_file.txt",
    ]
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
