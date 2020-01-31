package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "pending",
    deps = [
        ":module",
        "//base:debug",
        "//base:expected",
        "//base:graph",
        "//base:macros",
        "//diagnostic/consumer:streaming",
        "//frontend/source:source",
        "//frontend/source:file",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/container:node_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "module",
    deps = [
        ":assign_scope",
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast/scope:module",
        "//base:cast",
        "//base:macros",
        "//base:ptr_span",
        "//diagnostic/consumer",
        "//frontend:parse",
        "//frontend/source",
        "@com_google_absl//absl/algorithm:container",
    ],
    test_deps = None,
)

cc_lib(
    name = "dependent_decls",
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
        "//base:graph",
    ],
    test_deps = None,
)

cc_lib(
    name = "assign_scope",
    deps = [
        ":dependent_decls",
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
        "//ast/scope:decl",
        "//ast/scope:fn",
        "//ast/scope:scope",
    ],
    test_deps = None,
)
