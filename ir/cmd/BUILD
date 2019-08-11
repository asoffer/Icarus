package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "sources", "cc_lib_target")
sources()

cc_lib_target(
    name = "basic",
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
    name = "cast",
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
    name = "jumps",
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
    name = "load",
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
    name = "register",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//ir:basic_block",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "set_ret",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "@com_google_absl//absl/strings",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "store",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//ir:basic_block",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "types",
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
        "//backend:exec",
        "//base:util",
    ],
    impl_deps = None,
    test_deps = None,
)
