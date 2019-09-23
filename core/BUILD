package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "cc_component")

cc_component(name = "alignment", impl_deps = None, test_deps = None)
cc_component(name = "bytes", impl_deps = None, test_deps = None)

cc_component(
    name = "arch",
    intf_deps = [
        ":alignment",
        ":bytes",
        "//ir:addr",
        "//ir:any_func",
    ],
    impl_deps = None,
    test_deps = [],
)

cc_component(
    name = "builtin",
    textual_hdrs = ["builtin.xmacro.h"],
    intf_deps = ["//base:debug"],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "fn_args",
    intf_deps = [
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = None,
    test_deps = [],
)

cc_component(
    name = "fn_params",
    intf_deps = [
        ":fn_args",
        "//base:debug",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = None,
    test_deps = [],
)

cc_component(
    name = "ordered_fn_args",
    intf_deps = [
        ":fn_args",
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
    impl_deps = None,
    test_deps = [],
)

cc_lib_target(
    name = "pending_module",
    intf_deps = ["//base:expected"],
    impl_deps = [
        "//base:debug",
        "//base:graph",
        "//base:macros",
        "//misc:module", # TODO remove this.
        "@com_google_absl//absl/container:node_hash_set",
    ],
)

cc_lib_target(
    name = "scope",
    intf_deps = [
        "//ast:ast_fwd",
        "//base:util",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        "//misc:module", # TODO get rid of hard dependency here?
    ],
)
