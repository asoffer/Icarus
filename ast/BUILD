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
        "//core:arguments",
        "//core:dependency_node",
        "//core:parameters",
        "//core/type_system:type",
        "//frontend/lex:operators",
        "//data_types:addr",
        "//data_types:char",
        "//data_types:label",
        "//semantic_analysis:type_system",
        "@asoffer_nth//nth/meta:type",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/container:node_hash_map",
        "@com_google_absl//absl/functional:bind_front",
        "@com_google_absl//absl/numeric:int128",
    ],
)

cc_library(
    name = "expression",
    hdrs = ["expression.h"],
    deps = [
        ":node",
        "//data_types:hashtag",
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
        "//base:visitable",
        "@asoffer_nth//nth/meta:sequence",
        "@asoffer_nth//nth/meta:type",
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
        "@asoffer_nth//nth/utility:iterator_range",
        "@asoffer_nth//nth/utility:ptr_union",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)
