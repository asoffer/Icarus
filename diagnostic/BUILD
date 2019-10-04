package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component")

cc_component(
    name = "diagnostic",
    intf_deps = [
        "//frontend/source",
        "//frontend/source:range",
        "@com_google_absl//absl/strings:str_format",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "console_renderer",
    intf_deps = [":diagnostic"],
    impl_deps = [],
    test_deps = None,
)
