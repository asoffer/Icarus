package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "dump",
    deps = [
        "//ast:ast_fwd",
        "//ast:visitor",
        "//base:stringify",
        "//ast:ast",
        "//frontend:token",
        "//ir:results",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None,
)
