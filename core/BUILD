package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "sources")
sources()

cc_group_target(name = "impl",
                deps = [":scope-impl", ":pending_module-impl"])

cc_lib_target(name = "alignment", impl_deps = None, test_deps = None)
cc_lib_target(name = "bytes", impl_deps = None, test_deps = None)

cc_lib_target(
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

cc_lib_target(
    name = "fn_args",
    intf_deps = [
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = None,
    test_deps = ["//base:untyped_buffer"],
)

cc_lib_target(
    name = "fn_params",
    intf_deps = [
        ":fn_args",
        "//base:debug",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = None,
    test_deps = ["//base:untyped_buffer"],
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
        "//base:util",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        "//ast:declaration",
        "//ast:identifier",
        "//misc:module",
    ],
)
