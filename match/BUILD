package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component")

cc_component(
    name = "match_expr",
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:visitor",
    ],
    impl_deps = [
        ":binding_node",
        "//ast:ast",
    ],
)

cc_component(
    name = "binding_id",
    intf_deps = [],
    impl_deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
    ],
    test_deps = [],
)

cc_component(
    name = "binding_node",
    intf_deps = [
        ":binding_id",
        "//ast:expression",
        "//ast/methods:dump",
        "//module:assign_scope",
    ],
    impl_deps = None,
    test_deps = None,
)
