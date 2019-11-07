package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component")

cc_component(
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

cc_component(
    name = "decl",
    intf_deps = [":scope"],
    impl_deps = None,
)

cc_component(
    name = "scope_lit",
    intf_deps = [":scope"],
    impl_deps = None,
)


cc_component(
    name = "exec",
    intf_deps = [":scope"],
    impl_deps = [":fn"],
)


cc_component(
    name = "fn",
    intf_deps = [
        ":scope",
        "//base:bag",
    ],
    impl_deps = None,
)

cc_component(
    name = "module",
    intf_deps = [
        ":decl",
        ":scope",
    ],
    impl_deps = None,
)
