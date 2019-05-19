package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "sources")
sources()

# Directory of things I haven't totally figured out how to organize yet.

cc_group_target(
    name = "impl",
    deps = [
        ":constant_binding-impl",
        ":context-impl",
        ":module-impl",
    ])

cc_lib_target(
    name = "constant_binding",
    intf_deps = [
        "//base:untyped_buffer",
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
    name = "context",
    intf_deps = [
        ":module",
        "//base:debug",
        "//ir:register",
    ],
    impl_deps = [
        "//ast:expression",
        "//ast:declaration",
        "//ast:dispatch_table", #TODO not for formatting
    ],
    test_deps = None,
)

cc_lib_target(
    name = "module",
    intf_deps = [
        ":constant_binding",
        "//ast:dispatch_table", #TODO not for formatting
        "//ast:expression",
        "//ast:statements",
        "//core:fn_params",
        "//core:scope",
        "//error:log",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/container:node_hash_map",
    ],
    impl_deps = [
        "//ast:declaration",
        "//ast:expression",
        "//ast:function_literal",
        "//ast:struct_literal",
        "//backend:eval",
        "//base:guarded",
        "//frontend:source-impl",
        "//ir:compiled_fn",
        "//type:function",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "compile",
    intf_deps = None,
    impl_deps = [
        ":context",
        ":module",
        "//ast:declaration",
        "//backend:eval",
        "//frontend:source",
        "//ir:compiled_fn",
    ],
    test_deps = None,
)
