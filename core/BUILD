package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_lib(
    name = "impl",
    deps = [
        ":scope-impl",
    ],
)

cc_lib(
    name = "fn_args",
    hdrs = ["fn_args.h"],
    deps = [],
)

cc_test(
    name = "fn_args_test",
    srcs = ["fn_args_test.cc"],
    deps = [
        ":fn_args",
        "//test",
    ],
)

cc_target(
    name = "scope",
    intf_deps = [
        "//base:util",
        "//type:typed_value",
    ],
    impl_deps = [
        "//ast:declaration",
        "//ast:identifier",
        "//ast:match_declaration",
        "//ir:cmd",
        "//misc:context",
        "//misc:module",
        "//type:function",
        "//type:pointer",
    ],
)
