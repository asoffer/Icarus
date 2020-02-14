package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_library(name = "node_xmacro", textual_hdrs = ["node.xmacro.h"])

cc_lib(
    name = "ast_fwd",
    deps = [":node_xmacro"],
    header_only = True,
    test_deps = None,
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
        "//core:fn_params",
        "//core:ordered_fn_args",
        "//frontend/lex:operators",
        "//ir/value:addr",
        "//ir/value:label",
        "//ir:results",
        "//type:basic_type",
    ],
)

cc_lib(
    name = "expression",
    deps = [
        ":expr_ptr",
        ":hashtag",
        ":node",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "node",
    deps = [
        ":visitor_base",
        "//base:cast",
        "//frontend/source:range",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "expr_ptr",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "hashtag",
    deps = ["//base:debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "overload_set",
    deps = [
        "//ast:ast",
        "//ast/scope:scope",
        "//module:module",
        "@com_google_absl//absl/types:span",
    ],
    test_deps = None,
)

cc_lib(
    name = "visitor_base",
    deps = [
        ":ast_fwd",
        "//base:debug",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "visitor",
    deps = [
        ":ast",
        ":visitor_base",
    ],
    header_only = True,
    test_deps = None,
)
