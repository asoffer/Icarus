package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "sources")
sources()

# TODO rename ast_visitors
# TODO awful hack
cc_library(
    name = "visitors-compile",
    textual_hdrs = [
        "assign_scope.xmacro.h",
        "emit_ir.xmacro.h",
        "visitors.xmacro.h",
    ],
    defines = ["ICARUS_VISITOR_EMIT_IR"],
    alwayslink = True,
    deps = [
        ":assign_scope-compile",
        ":format-compile",
        ":dependent_decls-compile",
        ":extract_jumps-compile",
        ":emit_ir-compile",
        ":verify_type-compile",
    ])
cc_library(
    name = "visitors-format",
    textual_hdrs = [
        "assign_scope.xmacro.h",
        "emit_ir.xmacro.h",
        "visitors.xmacro.h",
    ],
    alwayslink = True,
    deps = [
        ":assign_scope-format",
        ":format-format",
        ":dependent_decls-format",
        ":extract_jumps-format",
        ":emit_ir-format",
        ":verify_type-format",
    ])


# TODO rename ast_visitors-impl
# TODO awful hack
cc_library(
    name = "visitors-impl-compile",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = [
        ":assign_scope-impl-compile",
        ":dependent_decls-impl-compile",
        ":emit_ir_copy_init-impl-compile",
        ":emit_ir_move_init-impl-compile",
        ":emit_ir_ref-impl-compile",
        ":emit_ir_val-impl-compile",
        ":extract_jumps-impl-compile",
        ":format-impl-compile",
        ":verify_type-impl-compile",
    ],
    defines = ["ICARUS_VISITOR_EMIT_IR"],
    alwayslink = True)
cc_library(
    name = "visitors-impl-format",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = [
        ":assign_scope-impl-format",
        ":dependent_decls-impl-format",
        ":emit_ir_copy_init-impl-format",
        ":emit_ir_move_init-impl-format",
        ":emit_ir_ref-impl-format",
        ":emit_ir_val-impl-format",
        ":extract_jumps-impl-format",
        ":format-impl-format",
        ":verify_type-impl-format",
    ],
    alwayslink = True)

# TODO hack mirroring -impl, -test
cc_group_target(
    name = "type_visitors",
    textual_hdrs = ["type_visitors.xmacro.h"],
    deps = [":emit_ir"])
cc_group_target(
    name = "type_visitors-impl",
    textual_hdrs = ["type_visitors.xmacro.h"],
    deps = [
        ":emit_init-impl",
        ":emit_assign-impl",
        ":emit_destroy-impl",
        ":emit_print-impl",
    ])

cc_lib_target(
    name = "assign_scope",
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:node_xmacro",
        "//core:scope",
    ],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)

cc_lib_target(
    name = "dependent_decls",
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
    intf_deps = [
        "//ast:ast_fwd",
        "//ir:addr",
        "//ir:register",
        "//type:type_fwd",
        "//type:typed_value",
    ],
    impl_deps = [],
    test_deps = [],
)

cc_lib_target(
    name = "emit_ir_copy_init",
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
    intf_deps = None,
    impl_deps = [
        ":emit_ir",
        "//ast:ast",
        "//backend:eval",
        "//base:guarded",
        "//core:pending_module",
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
    name = "verify_type",
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
