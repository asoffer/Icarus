package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "test",
    deps = [
        ":catch",
        ":elements_are",
    ],
    testonly = True,
)

cc_lib(
    name = "catch",
    hdrs = [
        "catch.h",
        "Catch2/single_include/catch2/catch.hpp",
    ],
    srcs = ["catch.cc"],
    deps = ["//base:macros"],
    testonly = True,
)

cc_lib(
    name = "elements_are",
    hdrs = ["elements_are.h"],
    deps = [":catch"],
    testonly = True,
)

cc_library(
    name = "util",
    hdrs = ["util.h"],
    deps = [
        "//misc:context",
        "//misc:module",
        "//:impl",
    ],
    testonly = True,
)
