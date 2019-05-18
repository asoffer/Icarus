package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
sources()

# TODO rename ast_visitors
cc_lib(
    name = "visitors",
    textual_hdrs = [
        "assign_scope.xmacro.h",
        "emit_ir.xmacro.h",
        "visitors.xmacro.h",
    ],
    deps = [
        ":assign_scope",
        ":format",
        ":dependent_decls",
        ":extract_jumps",
        ":emit_ir",
        ":verify_type",
    ],
)

# TODO rename ast_visitors-impl
cc_lib(
    name = "visitors-impl",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = [
        ":assign_scope-impl",
        ":dependent_decls-impl",
        ":emit_ir_copy_init",
        ":emit_ir_move_init",
        ":emit_ir_ref",
        ":emit_ir_val",
        ":extract_jumps-impl",
        ":format-impl",
        ":verify_type-impl",
    ],
)

cc_lib(
    name = "type_visitors",
    textual_hdrs = ["type_visitors.xmacro.h"],
    deps = [
        ":emit_ir",
    ],
)

cc_lib(
    name = "type_visitors-impl",
    textual_hdrs = ["type_visitors.xmacro.h"],
    deps = [
        ":emit_init",
        ":emit_assign",
        ":emit_destroy",
        ":emit_print",
    ],
)

cc_target(
    name = "assign_scope",
    intf_deps = [
        "//ast:ast-fwd",
        "//ast:node_xmacro",
        "//core:scope",
    ],
    impl_deps = ["//ast"],
)

cc_target(
    name = "dependent_decls",
    intf_deps = [
        "//ast:ast-fwd",
        "//ast:node_xmacro",
        "//base:graph",
    ],
    impl_deps = ["//ast"],
)

cc_lib(
    name = "emit_assign",
    srcs = ["emit_assign.cc"],
    deps = [
        ":special_function",
        "//ast",
        "//base:permutation",
        "//ir:cmd",
        "//ir:compiled_fn",
        "//ir:components",
        "//misc:context",
        "//type",
        "//type:typed_value",
    ],
)

cc_lib(
    name = "emit_destroy",
    srcs = ["emit_destroy.cc"],
    deps = [
        ":special_function",
        "//ast",
        "//base:permutation",
        "//ir:cmd",
        "//ir:compiled_fn",
        "//ir:components",
        "//misc:context",
        "//type",
        "//type:typed_value",
    ],
)

cc_lib(
    name = "emit_init",
    srcs = ["emit_init.cc"],
    deps = [
        "//ast",
        "//base:permutation",
        "//ir:cmd",
        "//ir:compiled_fn",
        "//ir:components",
        "//misc:context",
        "//type",
        "//type:typed_value",
    ],
)

cc_lib(
    name = "emit_ir",
    hdrs = ["emit_ir.h"],
    deps = [
        "//ast:ast-fwd",
        "//ir:addr",
        "//ir:register",
        "//type:type-fwd",
        "//type:typed_value",
    ],
)

cc_lib(
    name = "emit_ir_copy_init",
    srcs = ["emit_ir_copy_init.cc"],
    deps = [
        ":emit_ir",
        "//ast",
        "//ir:cmd",
        "//type",
        "//type:typed_value",
    ],
)

cc_lib(
    name = "emit_ir_move_init",
    srcs = ["emit_ir_move_init.cc"],
    deps = [
        ":emit_ir",
        "//ast",
        "//ir:cmd",
        "//type",
        "//type:typed_value",
    ],
)

cc_lib(
    name = "emit_ir_ref",
    srcs = ["emit_ir_ref.cc"],
    deps = [
        ":emit_ir",
        "//ast",
        "//backend:eval",
        "//ir:addr",
        "//ir:cmd",
        "//ir:register",
        "//ir:str",
        "//type",
        "//type:typed_value",
    ],
)

cc_lib(
    name = "emit_ir_val",
    srcs = ["emit_ir_val.cc"],
    deps = [
        ":emit_ir",
        "//ast",
        "//backend:eval",
        "//base:guarded",
        "//core:pending_module",
        "//ir:cmd",
        "//ir:components",
        "//ir:phi",
        "//ir:results",
        "//type",
        "//type:typed_value",
    ],
)

cc_lib(
    name = "emit_print",
    srcs = ["emit_print.cc"],
    deps = [
        "//ast",
        "//ir:arguments",
        "//ir:cmd",
        "//ir:compiled_fn",
        "//ir:components",
        "//misc:context",
        "//type",
        "//type:typed_value",
    ],
)


cc_target(
    name = "extract_jumps",
    intf_deps = [
        "//ast:ast-fwd",
        "//ast:node_xmacro",
    ],
    impl_deps = ["//ast"],
)


cc_target(
    name = "format",
    intf_deps = [
        "//ast:ast-fwd",
        "//ast:node_xmacro",
    ],
    impl_deps = [
        "//ast",
        "//base:log",
    ],
)

cc_target(
    name = "verify_type",
    intf_deps = [
        "//ast:ast-fwd",
        "//ast:node_xmacro",
    ],
    impl_deps = [
        ":extract_jumps",
        "//ast",
        "//ast:overload_set",
        "//backend:eval",
        "//misc:context",
        "//ir:cmd-impl",
        "//type",
        "//type:cast",
        "//type:util",
    ],
)

cc_target(
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
)
