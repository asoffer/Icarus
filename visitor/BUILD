package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "sources")
sources()

# TODO rename ast_visitors
# TODO hack mirroring -impl, -test
cc_group_target(
    name = "visitors",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = {
        "compile": [
            ":assign_scope",
            ":dependent_decls",
            ":dump_ast",
            ":extract_jumps",
            ":emit_ir",
            ":verify_type",
        ],
        "format": [
            ":dump_ast",
            ":format",
        ],
        "match": [
            ":assign_scope",
            ":dependent_decls",
            ":dump_ast",
            ":extract_jumps",
            ":emit_ir",
            ":match",
            ":verify_type",
        ],

    }
)

# TODO rename ast_visitors-impl
# TODO hack mirroring -impl, -test
cc_group_target(
    name = "visitors-impl",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = {
        "compile": [
            ":assign_scope-impl",
            ":dependent_decls-impl",
            ":dump_ast-impl",
            ":emit_ir_copy_init-impl",
            ":emit_ir_move_init-impl",
            ":emit_ir_ref-impl",
            ":emit_ir_val-impl",
            ":extract_jumps-impl",
            ":verify_type-impl",
        ],
        "format": [
            ":dump_ast-impl",
            ":format-impl",
        ],
        "match": [
            ":assign_scope-impl",
            ":dependent_decls-impl",
            ":dump_ast-impl",
            ":emit_ir_copy_init-impl",
            ":emit_ir_move_init-impl",
            ":emit_ir_ref-impl",
            ":emit_ir_val-impl",
            ":extract_jumps-impl",
            ":match-impl",
            ":verify_type-impl",
        ],
    }
)

# TODO hack mirroring -impl, -test
cc_group_target(
    name = "type_visitors",
    textual_hdrs = ["type_visitors.xmacro.h"],
    deps = {
        "compile": [
            ":emit_ir",
            ":type_query",
        ],
        "match": [
            ":emit_ir",
            ":type_query",
        ]
    },
)
cc_group_target(
    cfgs = ["match", "compile"],
    name = "type_visitors-impl",
    textual_hdrs = ["type_visitors.xmacro.h"],
    deps = [
        ":emit_init-impl",
        ":emit_assign-impl",
        ":emit_destroy-impl",
        ":emit_print-impl",
        ":type_query-impl",
    ])

cc_lib_target(
    name = "assign_scope",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:node_xmacro",
        "//core:scope",
    ],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)

cc_lib_target(
    name = "dump_ast",
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:node_xmacro",
    ],
    impl_deps = [
        "//base:stringify",
        "//ast:ast",
        "//ir:results",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "dependent_decls",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:node_xmacro",
        "//base:graph",
    ],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)

cc_lib_target(
    name = "emit_assign",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        ":special_function",
        "//ast:ast",
        "//base:permutation",
        "//ir:cmd",
        "//ir:compiled_fn",
        "//ir:components",
        "//misc:context",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_destroy",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        ":special_function",
        "//ast:ast",
        "//base:permutation",
        "//ir:cmd",
        "//ir:compiled_fn",
        "//ir:components",
        "//misc:context",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_init",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        "//ast:ast",
        "//base:permutation",
        "//ir:cmd",
        "//ir:compiled_fn",
        "//ir:components",
        "//misc:context",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_ir",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:ast_fwd",
        "//ir:addr",
        "//ir:register",
        "//type:type_fwd",
        "//type:typed_value",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "emit_ir_copy_init",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        ":emit_ir",
        "//ast:ast",
        "//ir:cmd",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_ir_move_init",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        ":emit_ir",
        "//ast:ast",
        "//ir:cmd",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_ir_ref",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        ":emit_ir",
        "//ast:ast",
        "//backend:eval",
        "//ir:addr",
        "//ir:cmd",
        "//ir:register",
        "//ir:str",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_ir_val",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        ":emit_ir",
        "//ast:ast",
        "//backend:eval",
        "//base:guarded",
        "//core:pending_module",
        "//ir:builtin_ir",
        "//ir:cmd",
        "//ir:components",
        "//ir:phi",
        "//ir:results",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_print",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        "//ast:ast",
        "//ir:arguments",
        "//ir:cmd",
        "//ir:compiled_fn",
        "//ir:components",
        "//misc:context",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)


cc_lib_target(
    name = "extract_jumps",
    cfgs = ["match", "compile"],
    intf_deps = ["//ast:ast_fwd", "//ast:node_xmacro"],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)


cc_lib_target(
    name = "format",
    intf_deps = ["//ast:ast_fwd", "//ast:node_xmacro"],
    impl_deps = ["//ast:ast", "//base:log"],
    test_deps = None,
)

cc_lib_target(
    name = "match",
    cfgs = ["match"],
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:node_xmacro",
    ],
    impl_deps = [
        ":match_state",
        "//match:binding_node",
        "//ast:ast",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "match_state",
    intf_deps = ["//ast:ast_fwd"],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "verify_type",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:node_xmacro",
    ],
    impl_deps = [
        ":extract_jumps",
        "//ast:ast",
        "//ast:overload_set",
        "//backend:eval",
        "//misc:context",
        "//misc:compile-impl",
        "//ir:cmd-impl",
        "//type:type",
        "//type:cast",
        "//type:util",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "special_function",
    cfgs = ["match", "compile"],
    intf_deps = [],
    impl_deps = [
        ":emit_ir",
        "//ast:declaration",
        "//ir:any_func",
        "//misc:context",
        "//type:pointer",
        "//type:struct",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "type_query",
    cfgs = ["match", "compile"],
    intf_deps = ["//type:type_fwd"],
    impl_deps = [
        "//ast:declaration",
        "//misc:context",
        "//type:all_types",
        "@com_google_absl//absl/algorithm:container",
    ],
    test_deps = None,
)
