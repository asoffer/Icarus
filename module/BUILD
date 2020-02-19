package(default_visibility = ["//visibility:public"])

cc_library(
    name = "pending",
    hdrs = ["pending.h"],
    srcs = ["pending.cc"],
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
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
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
)

cc_library(
    name = "dependent_decls",
    hdrs = ["dependent_decls.h"],
    srcs = ["dependent_decls.cc"],
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
        "//base:graph",
    ],
)

cc_library(
    name = "assign_scope",
    hdrs = ["assign_scope.h"],
    srcs = ["assign_scope.cc"],
    deps = [
        ":dependent_decls",
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
        "//ast/scope:decl",
        "//ast/scope:fn",
        "//ast/scope:scope",
    ],
)
