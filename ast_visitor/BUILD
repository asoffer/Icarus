package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
sources()

cc_lib(
    name = "visitors",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = [
        ":assign_scope",
        ":dependent_decls",
        ":extract_jumps",
        ":verify_type",
    ],
)

cc_lib(
    name = "visitors-impl",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = [
        ":assign_scope-impl",
        ":dependent_decls-impl",
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
    ],
)
