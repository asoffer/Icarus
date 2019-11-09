package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "diagnostic",
    deps = [
        "//frontend/source",
        "//frontend/source:range",
        "@com_google_absl//absl/strings:str_format",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "console_renderer",
    deps = [":diagnostic"],
    test_deps = None,
)
