package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target")

cc_lib_target(
    name = "extract_jumps",
    cfgs = ["match", "compile"],
    intf_deps = ["//ast:visitor"],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)


cc_lib_target(
    name = "type_query",
    cfgs = ["match", "compile"],
    intf_deps = ["//type:type_fwd"],
    impl_deps = [
        "//ast:ast",
        "//type:concrete_types",
        "@com_google_absl//absl/algorithm:container",
    ],
    test_deps = None,
)
