package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "sources")
sources()

# Directory of things I haven't totally figured out how to organize yet.

cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = [
        ":constant_binding-impl",
        ":context-impl",
        ":module-impl",
        "//visitor:assign_scope",
        "//visitor:emit_ir",
        "//visitor:verify_type",
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
    cfgs = ["match", "compile"],
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

COMMON_MODULE_INTF_TARGETS = [
    ":constant_binding",
    "//ast:expression",
    "//core:fn_params",
    "//core:pending_module",
    "//core:scope",
    "//error:log",
    "//type:typed_value",
    "@com_google_absl//absl/container:flat_hash_map",
    "@com_google_absl//absl/container:flat_hash_set",
    "@com_google_absl//absl/container:node_hash_map",
]

cc_group_target(
    name = "module",
    hdrs = ["module.h"],
    deps = {
        "compile": COMMON_MODULE_INTF_TARGETS + ["//ast:dispatch_table", "//ir:scope_def"],
        "format": COMMON_MODULE_INTF_TARGETS,
        "match": COMMON_MODULE_INTF_TARGETS + ["//ast:dispatch_table", "//ir:scope_def"],
    },
)

COMMON_MODULE_IMPL_TARGETS = [
    ":module",
    "//ast:declaration",
    "//ast:expression",
    "//ast:function_literal",
    "//ast:struct_literal",
    "//base:guarded",
    "//frontend:source-impl",
    "//type:function",
    "//type:jump",
]

cc_group_target(
    name = "module-impl",
    srcs = ["module.cc"],
    deps = {
        "compile": COMMON_MODULE_IMPL_TARGETS + ["//ir:compiled_fn"],
        "format": COMMON_MODULE_IMPL_TARGETS,
        "match": COMMON_MODULE_IMPL_TARGETS + ["//ir:compiled_fn"],
    },
)

cc_lib_target(
    name = "compile",
    cfgs = ["match", "compile"],
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
