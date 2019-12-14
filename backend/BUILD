package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "eval",
    deps = [
        "//base:debug",
        "//base:untyped_buffer",
        "//core:arch",
        "//interpretter:execute",
        "//ir:compiled_fn",
        "//ir:results",
    ],
    test_deps = None,
)
