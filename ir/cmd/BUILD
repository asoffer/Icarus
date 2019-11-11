package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target")

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
    name = "call",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "//ir:reg",
        "//ir:results",
        "//type:util",
        "@com_google_absl//absl/strings",
    ],
    impl_deps = [
        "//backend:exec",
    ],
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
        "@com_google_absl//absl/types:span",
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
    name = "misc",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//base:debug",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_lib_target(
    name = "phi",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//ir:basic_block",
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
    impl_deps = [],
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
    name = "return",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "//type:jump",
        "@com_google_absl//absl/strings",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "scope",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":util",
        "//compiler:compiler",
        "//ir:basic_block",
        "//ir:block_def",
    ],
    impl_deps = [
        "//base:stringify",
        "@com_google_absl//absl/strings",
    ],
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
        "//type:enum",
        "//type:flags",
        "//type:pointer",
        "//type:variant",
        "@com_google_absl//absl/random:random",
        "@com_google_absl//absl/random:distributions",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_lib_target(
    name = "util",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ir:builder",
        "//ir:values",
        "//backend:exec",
        "//base:tag",
        "//type:primitive",
        "//type:util",
    ],
    impl_deps = None,
    test_deps = None,
)

CMDS = [
    ":basic",
    ":call-impl",
    ":cast",
    ":jumps",
    ":load",
    ":misc-impl",
    ":phi",
    ":print-impl",
    ":register",
    ":return",
    ":scope",
    ":store",
    ":types",
]

cc_lib_target(
    name = "inline",
    cfgs = ["match", "compile"],
    intf_deps = ["//ir:inliner"] + CMDS,
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "execute",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//backend:exec",
        "//base:untyped_buffer",
        "//ir:addr",
    ] + CMDS,
    impl_deps = None,
    test_deps = None,
)
