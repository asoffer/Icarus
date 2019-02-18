package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "test",
    hdrs = ["test.h"],
    deps = [
        "//base:log",
        "//base:stringify",
        "//base:matchers",
        "//init:signal",
        "//init:signal-impl",
    ],
    testonly = True,
)
