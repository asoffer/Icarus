package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_component")

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

cc_lib(
    name = "elements_are",
    deps = [],
    header_only = True,
    test_deps = None,
    testonly = True,
)

cc_lib(
    name = "ordered_elements_are",
    deps = [],
    header_only = True,
    test_deps = None,
    testonly = True,
)

cc_lib(
    name = "module",
    deps = [
        ":util",
        "//ast:ast",
        "//ast:expression",
        "//ast:overload_set",
        "//base:ptr_span",
        "//frontend/source:range",
        "//module:module",
    ],
    header_only = True,
    test_deps = None,
    testonly = True,
)

cc_component(
    name = "util",
    intf_deps = [
        "//frontend:parse",
        "//frontend/source:string",
        "//module:module",
        "//type:function",
    ],
    impl_deps = None,
    test_deps = None,
    testonly = True,
)
