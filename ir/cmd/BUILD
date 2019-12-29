package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_library(
    name = "jump",
    hdrs = ["jump.h"],
    deps = [
        "//base:stringify",
        "//base:util",
        "//ir:reg",
    ],
)

cc_lib(
    name = "util",
    deps = [
        "//ir:values",
        "//base:tag",
        "//type:primitive",
        "//type:util",
    ],
    header_only = True,
    test_deps = None,
)
