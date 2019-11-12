package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "match_expr",
    deps = [
        ":binding_node",
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
    ],
    test_deps = None,
)

cc_lib(
    name = "binding_id",
    deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
    ],
    test_deps = [],
)

cc_lib(
    name = "binding_node",
    deps = [
        ":binding_id",
        "//ast:expression",
        "//ast/methods:dump",
        "//module:assign_scope",
    ],
    header_only = True,
    test_deps = None,
)
