package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target")

# Directory of things I haven't totally figured out how to organize yet.

cc_lib_target(
    name = "combine_blocks",
    cfgs = ["match", "compile"],
    intf_deps = [],
    impl_deps = [
        "//base:log",
        "//ir:compiled_fn",
    ],
    test_deps = None,
)
