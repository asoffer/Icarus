package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")
sources()

cc_lib_target(
    name = "property",
    cfgs = ["compile"],
    intf_deps = [
        "//base:owned_ptr",
        "//base:util",
        "//ir:cmd",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_lib_target(
    name = "property_map",
    cfgs = ["compile"],
    intf_deps = [
        ":property",
        ":property_set",
        "//base:hash",
        "//base:owned_ptr",
        "//base:util",
        "//ir:cmd",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        "//ir:arguments",
        "//ir:compiled_fn",
        "//core:arch",
        "//type:function",
        "//type:generic_struct",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "property_set",
    cfgs = ["compile"],
    intf_deps = [
        "//base:bag",
        "//base:owned_ptr",
        "//base:util",
    ],
    impl_deps = [":property"],
    test_deps = None,
)
