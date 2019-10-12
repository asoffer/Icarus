package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target")

# Directory of things I haven't totally figured out how to organize yet.

cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = [
        ":constant_binding-impl",
        "//module:module-impl",
    ])

cc_lib_target(
    name = "constant_binding",
    intf_deps = [
        "//base:untyped_buffer",
        "//core:bytes",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//ir:results",
        "//core:arch",
        "//type:type",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "dependent_data",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:dispatch_table",
        "//ast:expr_ptr",
        "//compiler:verify_result",
        "//module:pending",
        "//ir:scope_def",
    ],
    impl_deps = None,
    test_deps = None,
)
