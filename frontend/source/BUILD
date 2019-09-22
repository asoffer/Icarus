package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")

cc_lib_target(
    name = "source",
    intf_deps = ["//base:expected"],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "file",
    intf_deps = [":source", "//base:expected"],
    impl_deps = ["@com_google_absl//absl/strings:str_format"],
    test_deps = [],
    test_data = [
        "testdata/empty_file.txt",
        "testdata/one_line_file.txt",
        "testdata/multi_line_file.txt",
    ]
)

cc_lib_target(
    name = "repl",
    intf_deps = [":source"],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "string",
    intf_deps = [":source"],
    impl_deps = None,
    test_deps = [],
)

cc_lib_target(
    name = "range",
    intf_deps = ["//base:debug", "//base:interval"],
    impl_deps = None,
    test_deps = None,
)


