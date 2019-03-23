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
    deps = [
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "fn_args_test",
    srcs = ["fn_args_test.cc"],
    deps = [
        ":fn_args",
        "//test",
    ],
)

cc_lib(
    name = "fn_params",
    hdrs = ["fn_params.h"],
    deps = [
        ":fn_args",
        "//base:debug",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "fn_params_test",
    srcs = ["fn_params_test.cc"],
    deps = [
        ":fn_params",
        "//test",
    ],
)

cc_target(
    name = "scope",
    intf_deps = [
        "//base:util",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
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
