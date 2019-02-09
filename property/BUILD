package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_target(
    name = "property",
    intf_deps = [
        "//base:owned_ptr",
        "//base:util",
        "//ir:cmd",
    ],
)

cc_target(
    name = "property_map",
    intf_deps = [
        ":property",
        ":property_set",
        "//base:hash",
        "//base:owned_ptr",
        "//base:util",
        "//ir:cmd",
    ],
    impl_deps = [
        "//misc:architecture",
        "//ir:arguments",
        "//ir:func",
        "//type:function",
        "//type:generic_struct",
    ],
)

cc_target(
    name = "property_set",
    intf_deps = [
        "//base:bag",
        "//base:owned_ptr",
        "//base:util",
    ],
    impl_deps = [":property"],
)
