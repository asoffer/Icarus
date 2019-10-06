package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "cc_component")

cc_library(
    name = "test",
    deps = [
        ":catch",
        ":elements_are",
        ":ordered_elements_are",
    ],
    testonly = True,
)

cc_library(
    name = "catch",
    hdrs = ["catch.h", "Catch2/single_include/catch2/catch.hpp"],
    srcs = ["catch.cc"],
    deps = ["//base:macros"],
    testonly = True,
)

cc_component(name = "elements_are",
             impl_deps = None,
             test_deps = None,
             testonly = True)

cc_component(name = "ordered_elements_are",
             impl_deps = None,
             test_deps = None,
             testonly = True)

cc_lib_target(
    name = "util",
    cfgs = ["compile", "match"],
    intf_deps = [
        "//frontend/source:string",
        "//module:module",
        "//:impl",
    ],
    impl_deps = None,
    test_deps = None,
    testonly = True,
)
