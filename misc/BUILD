package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
sources()

# Directory of things I haven't totally figured out how to organize yet.

cc_lib(
    name = "impl",
    deps = [
        ":constant_binding-impl",
        ":context-impl",
        ":module-impl",
        ":import_graph-impl",
    ],
)

cc_target(
    name = "constant_binding",
    intf_deps = [
        "//base:untyped_buffer",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//ir:results",
        "//core:arch",
        "//type",
    ],
)

cc_target(
    name = "context",
    intf_deps = [
        ":module",
        "//base:debug",
        "//ir:register",
    ],
    impl_deps = [
        "//ast:expression",
        "//ast:declaration",
        "//ast:dispatch_table",
    ],
)

cc_target(
    name = "import_graph",
    intf_deps = [
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_target(
    name = "module",
    intf_deps = [
        ":constant_binding",
        "//ast:dispatch_table",
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
        ":import_graph",
        "//ast:declaration",
        "//ast:expression",
        "//ast:function_literal",
        "//ast:struct_literal",
        # "//backend:emit",
        "//backend:eval",
        "//base:guarded",
        "//frontend:source-impl",
        "//ir:compiled_fn",
        "//type:function",
    ],
)
