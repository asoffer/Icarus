package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

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

cc_library(
    name = "fuzz",
    hdrs = ["fuzz.h"],
    deps = [],
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
        "//compiler",
        "//diagnostic/consumer:streaming",
        "//frontend/source:range",
        "//module:module",
    ],
    header_only = True,
    test_deps = None,
    testonly = True,
)

cc_lib(
    name = "util",
    deps = [
        "//diagnostic/consumer:aborting",
        "//frontend:parse",
        "//frontend/source:string",
        "//module:module",
        "//type:function",
    ],
    header_only = True,
    test_deps = None,
    testonly = True,
)
