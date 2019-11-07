package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "cc_component")

cc_library(name = "node_xmacro", textual_hdrs = ["node.xmacro.h"])

cc_component(
    name = "ast_fwd",
    intf_deps = [":node_xmacro"],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "ast",
    intf_deps = [
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
    impl_deps = None,
    test_deps = None,
)

cc_component(name = "expression",
              intf_deps = [
                  ":expr_ptr",
                  ":hashtag",
                  ":node",
              ],
              impl_deps = None,
              test_deps = None)

cc_component(
    name = "node",
    intf_deps = [
        ":visitor_base",
        "//ast/methods:dump",
        "//base:cast",
        "//frontend/source:range",
        "//module:assign_scope",
        "//module:dependent_decls",
    ],
)

cc_component(
    name = "expr_ptr",
    intf_deps = [],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "dispatch_table",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//base:expected",
        "//compiler:verify_result",
        "//core:fn_args",
        "//core:fn_params",
        "//ir:basic_block",
        "//ir:block_def",
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
        "//:impl",
        "//test:module",
        "//test:util",
        "//type:pointer",
        "//type:variant",
    ],
)

cc_component(name = "hashtag")

cc_component(
    name = "overload_set",
    intf_deps = [
        "//base:bag",
        "//ast:ast",
        "//ast/scope:scope",
        "@com_google_absl//absl/types:span",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_component(
    name = "visitor_base",
    intf_deps = [
        ":ast_fwd",
        "//base:debug",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_component(
    name = "visitor",
    intf_deps = [
        ":ast",
        ":visitor_base",
    ],
    impl_deps = None,
    test_deps = None,
)
