package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
sources()

cc_lib(
    name = "visitors",
    textual_hdrs = glob(["*.xmacro.h"]),
    deps = [
        ":assign_scope",
        ":dependent_decls",
        ":extract_jumps",
        ":emit_ir",
        ":verify_type",
    ],
)

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
        ":verify_type-impl",
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
    name = "emit_ir",
    hdrs = ["emit_ir.h"],
    deps = [
        "//ast:ast-fwd",
        "//ir:addr",
        "//ir:register",
        "//type",
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
    ],
)

cc_lib(
    name = "emit_ir_move_init",
    srcs = ["emit_ir_move_init.cc"],
    deps = [
        ":emit_ir",
        "//ast",
        "//ir:cmd",
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
    ],
)

cc_lib(
    name = "emit_ir_val",
    srcs = ["emit_ir_val.cc"],
    deps = [
        ":emit_ir",
        "//ast",
        "//backend:eval",
        "//core:pending_module",
        "//ir:cmd",
        "//ir:components",
        "//ir:phi",
        "//ir:results",
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
        "//type",
        "//type:cast",
        "//type:util",
    ],
)
