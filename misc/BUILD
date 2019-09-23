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
        "//visitor:emit_ir",
        "//visitor:verify_type",
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

COMMON_MODULE_INTF_TARGETS = [
    ":constant_binding",
    "//ast:expr_ptr",
    "//core:fn_params",
    "//core:pending_module",
    "//core:scope",
    "//frontend/source:source",
    "//ir:reg",
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
        "compile": COMMON_MODULE_INTF_TARGETS + [":dependent_data"],
        "format": COMMON_MODULE_INTF_TARGETS,
        "match": COMMON_MODULE_INTF_TARGETS + [":dependent_data"],
    },
)

COMMON_MODULE_IMPL_TARGETS = [
    ":module",
    "//ast:ast",
    "//ast:expression",
    "//base:guarded",
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
        "//ir:scope_def",
        "//visitor:verify_result",
    ],
    impl_deps = None,
    test_deps = None,
)
