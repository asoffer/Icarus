package(default_visibility = ["//visibility:public"])

cc_library(name = "node_xmacro", textual_hdrs = ["node.xmacro.h"])

cc_library(
    name = "ast_fwd",
    hdrs = ["ast_fwd.h"],
    deps = [":node_xmacro"],
)

cc_library(
    name = "ast",
    hdrs = ["ast.h"],
    srcs = ["debug_string.cc"],
    deps = [
        ":expression",
        ":node",
        "//ast/scope:decl",
        "//ast/scope:exec",
        "//ast/scope:fn",
        "//ast/scope:module",
        "//ast/scope:scope",
        "//ast/scope:scope_lit",
        "//base:graph",
        "//base:ptr_span",
        "//core:builtin",
        "//core:fn_args",
        "//core:params_ref",
        "//core:ordered_fn_args",
        "//frontend/lex:operators",
        "//ir/value:addr",
        "//ir/value:label",
        "//ir/value:string",
        "//ir:results",
        "//type:basic_type",
    ],
)

cc_library(
    name = "expression",
    hdrs = ["expression.h"],
    deps = [
        ":expr_ptr",
        ":hashtag",
        ":node",
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
    name = "expr_ptr",
    hdrs = ["expr_ptr.h"],
    deps = [],
)

cc_library(
    name = "hashtag",
    hdrs = ["hashtag.h"],
    deps = ["//base:debug"],
)

cc_library(
    name = "overload_set",
    hdrs = ["overload_set.h"],
    srcs = ["overload_set.cc"],
    deps = [
        "//ast:ast",
        "//ast/scope:scope",
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
    hdrs = ["visitor.h"],
    deps = [
        ":ast",
        ":visitor_base",
    ],
)
