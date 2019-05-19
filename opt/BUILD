package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "sources")
sources()

# Directory of things I haven't totally figured out how to organize yet.

cc_group_target(
    name = "impl",
    deps = [":combine_blocks-impl"])

cc_lib_target(
    name = "combine_blocks",
    intf_deps = [],
    impl_deps = [
        "//base:log",
        "//ir:compiled_fn",
    ],
    test_deps = None,
)
