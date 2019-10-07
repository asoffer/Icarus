package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "icarus_method")

icarus_method(
    name = "match_expr",
    cfgs = ["match"],
    intf_deps = [
        "//ast:ast_fwd",
        "//module:assign_scope-xmacro",
        "//module:dependent_decls-xmacro",
    ],
    impl_deps = [
        "//match:binding_node",
        "//ast:ast",
    ],
)

icarus_method(
    name = "ast_methods",
    deps = [
        "//ast/methods:dump-xmacro",
        ":match_expr-xmacro",
    ],
    gen = False,
)

cc_lib_target(
    cfgs = ["match"],
    name = "ast_dependencies",
    textual_hdrs = ["ast_dependencies.h"],
    intf_deps = [
        ":match_expr",
        "//module:assign_scope",
        "//module:dependent_decls",
        "//visitor:extract_jumps",
        "//ir:results",
        "//ast/methods:dump",
        "//compiler:compiler",
        "//compiler:ast_methods-xmacro",
        "//match:ast_methods-xmacro",
    ],
)

icarus_method(
    name = "type_methods",
    deps = [
        "//compiler:compiler-match",
        "//visitor:type_query-match",
    ],
    gen = False,
)

cc_lib_target(
    name = "type_dependencies",
    cfgs = ["match"],
    textual_hdrs = ["type_dependencies.h"],
    intf_deps = [
        "//compiler:compiler",
        "//visitor:type_query",
        "//match:type_methods-xmacro",
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
