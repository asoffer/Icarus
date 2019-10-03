package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target")

# Directory of things I haven't totally figured out how to organize yet.

cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = [
        ":constant_binding-impl",
        ":module-impl",
        "//visitor:assign_scope",
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
    name = "module",
    intf_deps = [
        "//core:pending_module",
        "//core:scope",
        "//error:log",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = ["//ast:ast"],
)

cc_lib_target(
    name = "compile",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        ":module",
        "//ast:ast",
        "//backend:eval",
        "//frontend/source:file-impl",
        "//ir:compiled_fn",
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
        "//core:pending_module",
        "//ir:scope_def",
    ],
    impl_deps = None,
    test_deps = None,
)
