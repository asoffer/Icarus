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
        "//frontend/source:buffer",
    ],
)

cc_library(
    name = "ast",
    hdrs = [
        "ast.h",
        "visitor.h"
    ],
    srcs = [
        "build_param_dependency_graph.h",
        "build_param_dependency_graph.cc",
        "debug_string.cc",
        "initialize.cc",
    ],
    deps = [
        ":scope",
        ":declaration",
        ":expression",
        ":jump_options",
        ":node",
        ":visitor_base",
        "//base:debug",
        "//base:graph",
        "//base:ptr_span",
        "//base:untyped_buffer",
        "//core:arguments",
        "//core:dependency_node",
        "//core:ordered_arguments",
        "//core:params",
        "//frontend/lex:operators",
        "//ir/value:addr",
        "//ir/value:builtin_fn",
        "//ir/value:char",
        "//ir/value:integer",
        "//ir/value:label",
        "//ir/value:result_buffer",
        "//ir/value:slice",
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
    name = "jump_options",
    hdrs = ["jump_options.h"],
    deps = [
        ":expression",
        "//core:arguments",
    ],
)

cc_library(
    name = "node",
    hdrs = ["node.h"],
    deps = [
        ":visitor_base",
        "//base:cast",
        "//frontend/source:buffer",
    ],
)

cc_library(
    name = "overload_set",
    hdrs = ["overload_set.h"],
    srcs = ["overload_set.cc"],
    deps = [
        ":ast",
        ":scope",
        "//base:log",
        "//module:module",
        "@com_google_absl//absl/types:span",
    ],
    test_deps = None,
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
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/types:span"
    ],
)

cc_library(
    name = "visitor_base",
    hdrs = ["visitor_base.h"],
    deps = [
        ":ast_fwd",
        "//base:debug",
    ],
)

cc_library(
    name = "visitor",
    hdrs = [],
    deps = [
        ":ast",
        ":visitor_base",
    ],
)
