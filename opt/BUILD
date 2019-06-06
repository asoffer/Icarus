package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "sources")
sources()

# Directory of things I haven't totally figured out how to organize yet.

cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = [
        ":combine_blocks-impl",
        ":combine_prints-impl",
    ])

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

cc_lib_target(
    name = "combine_prints",
    cfgs = ["match", "compile"],
    intf_deps = [],
    impl_deps = [
        "//ir:compiled_fn",
        "//ir:cmd",
        "//ir:str",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None,
)
