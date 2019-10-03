package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component")

cc_component(
    name = "source",
    intf_deps = ["//base:expected"],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "file",
    intf_deps = [
        ":source",
        "//base:expected",
    ],
    impl_deps = ["@com_google_absl//absl/strings:str_format"],
    test_deps = [],
    test_data = [
        "testdata/empty_file.txt",
        "testdata/one_line_file.txt",
        "testdata/multi_line_file.txt",
    ]
)

cc_component(
    name = "id",
    intf_deps = [],
    impl_deps = [],
    test_deps = []
)

cc_component(
    name = "repl",
    intf_deps = [":source"],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "string",
    intf_deps = [":source"],
    impl_deps = None,
    test_deps = [],
)

cc_component(
    name = "range",
    intf_deps = [
        "//base:debug",
        "//base:interval",
        "//base:strong_types",
    ],
    impl_deps = None,
    test_deps = None,
)
