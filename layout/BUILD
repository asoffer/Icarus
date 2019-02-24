package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_lib(name = "alignment", hdrs = ["alignment.h"])
cc_lib(name = "bytes", hdrs = ["bytes.h"])

cc_lib(
    name = "arch",
    hdrs = ["arch.h"],
    deps = [
        ":alignment",
        ":bytes",
        "//ir:addr",
        "//ir:any_func",
    ],
)

cc_test(
    name = "arch_test",
    srcs = ["arch_test.cc"],
    deps = [
        ":arch",
        "//test"
    ],
)
