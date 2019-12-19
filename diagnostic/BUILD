package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_library(
    name = "errors",
    hdrs = ["errors.h"],
    deps = [
        "//frontend/source",
        "//frontend/source:range",
        "//type:qual_type",
    ],
)

cc_lib(
    name = "message",
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
    deps = [
        ":message",
        "//base:util",
    ],
    test_deps = None,
)
