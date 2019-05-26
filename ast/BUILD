package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")
load(":defs.bzl", "icarus_ast_node")

sources()

# TODO this is a hack
cc_library(name = "node_xmacro-compile", textual_hdrs = ["node.xmacro.h"])
cc_library(name = "node_xmacro-format", textual_hdrs = ["node.xmacro.h"])
cc_library(name = "node_xmacro-match", textual_hdrs = ["node.xmacro.h"])

cc_lib_target(
    name = "ast_fwd",
    intf_deps = [":node_xmacro"],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "ast",
    intf_deps = [
        ":block_node",
        ":builtin_fn",
        ":call",
        ":cast",
        ":chainop",
        ":comma_list",
        ":declaration",
        ":enum_literal",
        ":expression",
        ":function_literal",
        ":identifier",
        ":import",
        ":index",
        ":interface",
        ":match_declaration",
        ":node",
        ":node_span",
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
    impl_deps = None,
    test_deps = None,
)

icarus_ast_node(name = "block_node",
                deps = [":expression", ":statements", "//core:scope"])
icarus_ast_node(name = "builtin_fn", deps = [":expression", "//ir:builtin"])
icarus_ast_node(name = "call", deps = [":expression"])
icarus_ast_node(name = "cast", deps = [":expression"])
icarus_ast_node(name = "chainop",
                deps = [":expression", "//frontend:operators"])
icarus_ast_node(name = "comma_list", deps = [":expression"])
icarus_ast_node(name = "declaration", deps = [":expression"])
icarus_ast_node(name = "enum_literal", deps = [":expression"])
icarus_ast_node(name = "expression", deps = [":hashtag", ":node"])
icarus_ast_node(name = "function_literal", deps = [
    ":declaration",
    ":expression",
    ":statements",
    "//base:graph",
    "//core:fn_params",
])
icarus_ast_node(name = "identifier", deps = [":expression"])
icarus_ast_node(name = "import", deps = [":expression", "//core:pending_module"])
icarus_ast_node(name = "index", deps = [":expression"])
icarus_ast_node(name = "interface", deps = [":expression"])
icarus_ast_node(name = "node", deps = [
    "//base:util",
    "//visitor:visitors",
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
icarus_ast_node(name = "terminal",
                deps = [":expression", "//ir:results", "//type:primitive"])
icarus_ast_node(name = "unop", deps = [":expression", "//frontend:operators"])

cc_lib_target(name = "node_span", intf_deps = [], impl_deps = None, test_deps = None)

cc_lib_target(
    name = "dispatch_table",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":overload_set",
        "//core:fn_args",
        "//base:expected",
        "//ir:block",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":ast",
        ":call",
        ":builtin_fn",
        ":function_literal",
        "//ast:match_declaration",
        "//backend:eval",
        "//ir:block",
        "//ir:cmd",
        "//ir:components",
        "//ir:compiled_fn",
        "//ir:phi",
        "//misc:context",
        "//misc:module",
        "//core:scope",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
    ],
    test_deps = [
        "//:impl",
        "//test:util",
        "//type:pointer",
        "//type:variant",
    ],
)

cc_lib_target(name = "hashtag")

cc_lib_target(
    name = "overload_set",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":node_span",
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
    test_deps = None,
)
