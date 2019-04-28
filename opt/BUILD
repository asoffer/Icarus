package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

# Directory of things I haven't totally figured out how to organize yet.

cc_lib(
    name = "impl",
    deps = [
        ":combine_blocks-impl",
    ],
)

cc_target(
    name = "combine_blocks",
    intf_deps = [
    ],
    impl_deps = [
        "//base:log",
        "//ir:compiled_fn",
    ],
)
