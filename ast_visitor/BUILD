package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
sources()

cc_lib(
    name = "visitors",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = [
        ":assign_scope",
        ":extract_jumps",
        ":verify_type",
    ],
)

cc_lib(
    name = "visitors-impl",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = [
        ":assign_scope-impl",
        ":extract_jumps-impl",
        ":verify_type-impl",
    ],
)

cc_target(
    name = "assign_scope",
    intf_deps = [
        "//ast:ast-fwd",
        "//core:scope",
    ],
    impl_deps = ["//ast"],
)

cc_target(
    name = "extract_jumps",
    intf_deps = ["//ast:ast-fwd"],
    impl_deps = ["//ast"],
)

cc_target(
    name = "verify_type",
    intf_deps = ["//ast:ast-fwd"],
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
