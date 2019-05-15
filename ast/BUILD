package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
load(":defs.bzl", "icarus_ast_node")

sources()

cc_lib(
    name = "node_xmacro",
    textual_hdrs = ["node.xmacro.h"],
)

cc_lib(
    name = "ast-fwd",
    hdrs = ["ast_fwd.h"],
    deps = [":node_xmacro"],
)

cc_lib(
    name = "ast",
    hdrs = ["ast.h"],
    deps = [
        ":access",
        ":array_literal",
        ":array_type",
        ":binop",
        ":block_literal",
        ":block_node",
        ":builtin_fn",
        ":call",
        ":cast",
        ":chainop",
        ":comma_list",
        ":declaration",
        ":dispatch_table",
        ":enum_literal",
        ":expression",
        ":function_literal",
        ":hashtag",
        ":identifier",
        ":import",
        ":index",
        ":interface",
        ":match_declaration",
        ":node",
        ":overload_set",
        ":repeated_unop",
        ":scope_literal",
        ":scope_node",
        ":statements",
        ":struct_literal",
        ":struct_type",
        ":switch",
        ":terminal",
        ":unop",
    ],
)

cc_lib(
    name = "impl",
    deps = [
        ":dispatch_table-impl",
        ":overload_set-impl",
    ],
)

icarus_ast_node(name = "access", deps = [":expression"])
icarus_ast_node(name = "array_literal", deps = [":expression", ":comma_list"])
icarus_ast_node(name = "array_type", deps = [":expression"])
icarus_ast_node(name = "binop", deps = [":expression", "//frontend:operators"])
icarus_ast_node(name = "block_literal", deps = [":expression", ":declaration"])
icarus_ast_node(name = "block_node", deps = [":expression", ":statements"])
icarus_ast_node(name = "builtin_fn", deps = [":expression", "//ir:builtin"])
icarus_ast_node(name = "call", deps = [":expression"])
icarus_ast_node(name = "cast", deps = [":expression"])
icarus_ast_node(name = "chainop",
                deps = [":expression", "//frontend:operators"])
icarus_ast_node(name = "comma_list", deps = [":expression"])
icarus_ast_node(name = "declaration", deps = [":expression"])
icarus_ast_node(name = "enum_literal", deps = [":expression"])
icarus_ast_node(name = "expression",
                deps = [":hashtag", ":node", "//type:util"])
icarus_ast_node(name = "function_literal", deps = [
    ":declaration",
    ":expression",
    ":statements",
    "//core:fn_params",
])
icarus_ast_node(name = "identifier", deps = [":expression"])
icarus_ast_node(name = "import", deps = [":expression", "//core:pending_module"])
icarus_ast_node(name = "index", deps = [":expression"])
icarus_ast_node(name = "interface", deps = [":expression"])
icarus_ast_node(name = "node", deps = [
    "//ast_visitor:visitors",
    "//frontend:text_span",
    "//ir:results",
])
icarus_ast_node(name = "match_declaration", deps = [":declaration"])
icarus_ast_node(name = "repeated_unop",
                deps = [":comma_list", "//frontend:operators"])
icarus_ast_node(name = "scope_literal", deps = [":declaration", ":expression"])
icarus_ast_node(name = "scope_node",
                deps = [":block_node", ":declaration", ":expression"])
icarus_ast_node(name = "statements", deps = [":node"])
icarus_ast_node(name = "struct_literal", deps = [":declaration", ":expression"])
icarus_ast_node(name = "struct_type", deps = [":expression"])
icarus_ast_node(name = "switch", deps = [":expression"])
icarus_ast_node(name = "terminal", deps = [":expression", "//ir:results"])
icarus_ast_node(name = "unop", deps = [":expression", "//frontend:operators"])

cc_target(
    name = "dispatch_table",
    intf_deps = [
        ":overload_set",
        "//core:fn_args",
        "//base:expected",
        "//ir:block",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":call",
        ":builtin_fn",
        ":block_literal",
        ":function_literal",
        "//ast:match_declaration",
        "//backend:eval",
        "//misc:context",
        "//ir:block",
        "//ir:cmd",
        "//ir:components",
        "//ir:compiled_fn",
        "//ir:phi",
        "//misc:module",
        "//core:scope",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
    ],
)

cc_test(
    name = "dispatch_table_test",
    srcs = ["dispatch_table_test.cc"],
    deps = [
        "//:impl",
        "//test:util",
        "//test",
    ],
)

cc_lib(
    name = "hashtag",
    hdrs = ["hashtag.h"],
)

cc_target(
    name = "overload_set",
    intf_deps = [
        "//base:bag",
        "//type:typed_value",
        "//type:function",
    ],
    impl_deps = [
        ":declaration",
        "//misc:context",
        "//core:scope",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)
