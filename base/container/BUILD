package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "bag",
    hdrs = ["bag.h"],
)

cc_lib(
    name = "map",
    hdrs = ["map.h"],
)

cc_lib(
    name = "tuple",
    hdrs = ["tuple.h"],
)

cc_lib(
    name = "unordered_map",
    hdrs = ["unordered_map.h"],
    deps = ["//base:log"],
)

cc_lib(
    name = "vector",
    hdrs = ["vector.h"],
)
