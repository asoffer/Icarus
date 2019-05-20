package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target")

cc_group_target(
    name = "test",
    deps = [":catch-impl", ":elements_are", ":ordered_elements_are"],
    testonly = True)

cc_lib_target(name = "catch",
              extra_hdrs = ["Catch2/single_include/catch2/catch.hpp"],
              intf_deps = ["//base:macros"],
              impl_deps = [],
              test_deps = None,
              testonly = True)

cc_lib_target(name = "elements_are",
              impl_deps = None,
              test_deps = None,
              testonly = True)
cc_lib_target(name = "ordered_elements_are",
              impl_deps = None,
              test_deps = None,
              testonly = True)

cc_lib_target(
    name = "util",
    cfgs = ["compile"],
    intf_deps = [
        "//misc:context",
        "//misc:module",
        "//:impl",
    ],
    impl_deps = None,
    test_deps = None,
    testonly = True,
)
