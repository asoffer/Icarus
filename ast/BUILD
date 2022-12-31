package(default_visibility = ["//visibility:public"])

cc_library(name = "node_xmacro", textual_hdrs = ["node.xmacro.h"])

cc_library(
    name = "ast_fwd",
    hdrs = ["ast_fwd.h"],
    deps = [":node_xmacro"],
)

cc_library(
    name = "declaration",
    hdrs = ["declaration.h"],
    deps = [
        ":expression",
        ":node",
    ],
)

cc_library(
    name = "ast",
    hdrs = [
        "ast.h",
    ],
    srcs = [
        "build_param_dependency_graph.h",
        "build_param_dependency_graph.cc",
        "debug_string.cc",
        "initialize.cc",
    ],
    deps = [
        ":declaration",
        ":expression",
        ":module",
        ":node",
        ":scope",
        "//base:debug",
        "//base:graph",
        "//base:ptr_span",
        "//base:untyped_buffer",
        "//core:arguments",
        "//core:dependency_node",
        "//core:parameters",
        "//core/type_system:type",
        "//frontend/lex:operators",
        "//ir/value:addr",
        "//ir/value:char",
        "//ir/value:integer",
        "//ir/value:label",
        "//semantic_analysis:type_system",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/container:node_hash_map",
        "@com_google_absl//absl/functional:bind_front",
    ],
)

cc_library(
    name = "expression",
    hdrs = ["expression.h"],
    deps = [
        ":node",
        "//ir/value:hashtag",
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    deps = [
        ":node",
        ":declaration",
        ":scope",
        "//base:ptr_span",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "node",
    hdrs = ["node.h"],
    deps = [
        ":node_xmacro",
        "//base:cast",
        "//base:meta",
        "//base:visitable",
    ],
)

cc_library(
    name = "scope",
    hdrs = ["scope.h"],
    srcs = ["scope.cc"],
    deps = [
        ":ast_fwd",
        ":declaration",
        "//base:cast",
        "//base:debug",
        "//base:log",
        "//base:iterator",
        "//base:ptr_union",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)
