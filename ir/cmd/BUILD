package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component", "cc_lib")

cc_component(
    name = "basic",
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "call",
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

cc_component(
    name = "cast",
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "jumps",
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
        "@com_google_absl//absl/types:span",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "load",
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "misc",
    intf_deps = [
        ":util",
        "//base:debug",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_component(
    name = "phi",
    intf_deps = [
        ":util",
        "//ir:basic_block",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "print",
    intf_deps = [
        ":util",
        "//ir:basic_block",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_component(
    name = "register",
    intf_deps = [
        ":util",
        "//ir:basic_block",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "return",
    intf_deps = [
        ":util",
        "//ir:basic_block",
        "//type:jump",
        "@com_google_absl//absl/strings",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "scope",
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

cc_component(
    name = "store",
    intf_deps = [
        ":util",
        "//ir:basic_block",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "types",
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

cc_lib(
    name = "util",
    deps = [
        "//ir:builder",
        "//ir:values",
        "//backend:exec",
        "//base:tag",
        "//type:primitive",
        "//type:util",
    ],
    header_only = True,
    test_deps = None,
)
