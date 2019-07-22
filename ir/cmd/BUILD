package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "sources", "cc_lib_target")
sources()

cc_lib_target(
    name = "arithmetic",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "print",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//ir:basic_block",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "util",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//base:util",
    ],
    impl_deps = None,
    test_deps = None,
)
