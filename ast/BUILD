package(default_visibility = ["//visibility:public"])

cc_library(name = "node_xmacro", textual_hdrs = ["node.xmacro.h"])

cc_library(
    name = "ast_fwd",
    hdrs = ["ast_fwd.h"],
    deps = [":node_xmacro"],
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
        "is_dependent.cc",
    ],
    deps = [
        ":expression",
        ":jump_options",
        ":node",
        ":visitor_base",
        "//ast/scope:decl",
        "//ast/scope:exec",
        "//ast/scope:fn",
        "//ast/scope:module",
        "//ast/scope:scope",
        "//ast/scope:scope_lit",
        "//base:graph",
        "//base:ptr_span",
        "//core:arguments",
        "//core:dependency_node",
        "//core:params_ref",
        "//core:ordered_arguments",
        "//frontend/lex:operators",
        "//ir/value:addr",
        "//ir/value:builtin_fn",
        "//ir/value:label",
        "//ir/value:string",
        "//ir/value",
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
        "//frontend/source:range",
    ],
)

cc_library(
    name = "overload_set",
    hdrs = ["overload_set.h"],
    srcs = ["overload_set.cc"],
    deps = [
        "//ast:ast",
        "//ast/scope:scope",
        "//base:log",
        "//module:module",
        "@com_google_absl//absl/types:span",
    ],
    test_deps = None,
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
