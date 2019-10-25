package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target")

cc_lib_target(
    name = "scope",
    intf_deps = [
        "//ast:ast_fwd",
        "//base:cast",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        "//ast:ast",
        "//ast/scope:exec",
        "//ast/scope:fn",
        "//module:module",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "decl",
    intf_deps = [":scope"],
    impl_deps = None,
)

cc_lib_target(
    name = "scope_lit",
    intf_deps = [":scope"],
    impl_deps = None,
)


cc_lib_target(
    name = "exec",
    intf_deps = [":scope"],
    impl_deps = [":fn"],
)


cc_lib_target(
    name = "fn",
    intf_deps = [
        ":scope",
        "//base:bag",
    ],
    impl_deps = None,
)

cc_lib_target(
    name = "module",
    intf_deps = [
        ":decl",
        ":scope",
    ],
    impl_deps = None,
)
