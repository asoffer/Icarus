package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "icarus_method")

icarus_method(
    name = "match_expr",
    cfgs = ["match"],
    intf_deps = [
        "//ast:ast_fwd",
        "//ast:visitor",
    ],
    impl_deps = [
        ":binding_node",
        "//ast:ast",
    ],
)

cc_lib_target(
    name = "binding_id",
    cfgs = ["match"],
    intf_deps = [],
    impl_deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
    ],
    test_deps = [],
)

cc_lib_target(
    name = "binding_node",
    cfgs = ["match"],
    intf_deps = [
        ":binding_id",
        "//ast:expression",
    ],
    impl_deps = None,
    test_deps = None,
)
