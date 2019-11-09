package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component", "cc_lib")

cc_library(name = "node_xmacro", textual_hdrs = ["node.xmacro.h"])

cc_lib(
    name = "ast_fwd",
    deps = [":node_xmacro"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "ast",
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
        "//frontend:operators",
        "//ir:block_def",
        "//ir:results",
        "//type:basic_type",
    ],
    header_only = True,
    test_deps = None,
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

cc_component(
    name = "dispatch_table",
    intf_deps = [
        "//base:expected",
        "//compiler:verify_result",
        "//core:fn_args",
        "//core:fn_params",
        "//ir:basic_block",
        "//ir:block_def",
        "//ir:inliner",
        "//ir:jump_handler",
        "//ir:results",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":ast",
        ":overload_set",
        "//ast/scope:scope",
        "//backend:eval",
        "//compiler:compiler",
        "//ir:block_def",
        "//ir:builder",
        "//ir/cmd:call",
        "//ir/cmd:phi",
        "//ir/cmd:store",
        "//ir:components",
        "//ir:compiled_fn",
        "//module:module",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
    ],
    test_deps = [
        "//test:module",
        "//test:util",
        "//type:pointer",
        "//type:variant",
    ],
)

cc_lib(
    name = "hashtag",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "overload_set",
    deps = [
        "//base:bag",
        "//ast:ast",
        "//ast/scope:scope",
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
