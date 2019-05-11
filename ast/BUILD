package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
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
        ":access-impl",
        ":array_literal-impl",
        ":array_type-impl",
        ":binop-impl",
        ":block_literal-impl",
        ":block_node-impl",
        ":builtin_fn-impl",
        ":call-impl",
        ":cast-impl",
        ":chainop-impl",
        ":comma_list-impl",
        ":declaration-impl",
        ":dispatch_table-impl",
        ":enum_literal-impl",
        ":expression-impl",
        ":function_literal-impl",
        ":identifier-impl",
        ":import-impl",
        ":index-impl",
        ":interface-impl",
        ":match_declaration-impl",
        ":overload_set-impl",
        ":repeated_unop-impl",
        ":scope_literal-impl",
        ":scope_node-impl",
        ":statements-impl",
        ":struct_literal-impl",
        ":struct_type-impl",
        ":switch-impl",
        ":unop-impl",
    ],
)

cc_target(
    name = "access",
    intf_deps = [":expression"],
    impl_deps = [
        ":declaration",
        "//backend:eval",
        "//ir:cmd",
        "//ir:components",
        "//misc:module",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "//type:struct",
    ]
)

cc_target(
    name = "array_literal",
    intf_deps = [
        ":literal",
        ":comma_list",
    ],
    impl_deps = [
        "//misc:context",
        "//error:log",
        "//ir:cmd",
        "//type:array",
        "//type:cast",
        "//type:pointer",
    ],
)

cc_target(
    name = "array_type",
    intf_deps = [":literal"],
    impl_deps = ["//ir:cmd"],
)

cc_target(
    name = "binop",
    intf_deps = [
        ":literal",
        "//frontend:operators",
    ],
    impl_deps = [
        ":comma_list",
        ":overload_set",
        "//backend:eval",
        "//core:fn_args",
        "//misc:context",
        "//ir:components",
        "//ir:compiled_fn",
        "//ir:phi",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
        "//type:variant",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_target(
    name = "block_literal",
    intf_deps = [
        ":declaration",
        ":literal",
    ],
    impl_deps = [
        "//ir:block",
        "//misc:context",
        "//core:scope",
        "//type:function",
        "//type:primitive",
    ],
)

cc_target(
    name = "block_node",
    intf_deps = [
        ":expression",
        ":statements",
        "//core:scope",
        "//misc:context",
    ],
    impl_deps = [],
)

cc_target(
    name = "builtin_fn",
    intf_deps = [
        ":literal",
        "//core:fn_args",
        "//ir:builtin",
        "//misc:context",
    ],
    impl_deps = [
        "//backend:eval",
        "//ir:arguments",
        "//ir:cmd",
    ],
)

cc_target(
    name = "call",
    intf_deps = [
        ":literal",
        "//core:fn_args",
    ],
    impl_deps = [
        ":block_literal",
        ":builtin_fn",
        ":dispatch_table",
        ":function_literal",
        ":struct_literal",
        ":unop",
        "//backend:eval",
        "//core:fn_params",
        "//ir:arguments",
        "//ir:components",
        "//ir:compiled_fn",
        "//ir:phi",
        "//core:scope",
        "//type:array",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_target(
    name = "cast",
    intf_deps = [":expression"],
    impl_deps = [
        ":overload_set",
        "//backend:eval",
        "//core:fn_args",
        "//ir:cmd",
        "//misc:context",
        "//type:cast",
        "//type:tuple",
    ],
)

cc_target(
    name = "chainop",
    intf_deps = [
        ":literal",
        "//frontend:operators",
    ],
    impl_deps = [
        ":overload_set",
        "//core:fn_args",
        "//misc:context",
        "//ir:compiled_fn",
        "//ir:phi",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_target(
    name = "comma_list",
    intf_deps = [":expression"],
    impl_deps = [
        "//misc:context",
        "//ir:cmd",
        "//type:tuple",
    ],
)

cc_target(
    name = "declaration",
    intf_deps = [
        ":expression",
        "//ir:register",
    ],
    impl_deps = [
        ":function_literal",
        "//backend:eval",
        "//error:inference_failure_reason",
        "//ir:compiled_fn",
        "//misc:module",
        "//type:cast",
        "//type:typed_value",
    ],
)

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

cc_target(
    name = "enum_literal",
    intf_deps = [
        ":declaration",
        ":literal",
        "//core:scope",
    ],
    impl_deps = [
        ":identifier",
        "//misc:context",
        "//ir:cmd",
    ],
)


cc_target(
    name = "expression",
    intf_deps = [
        ":hashtag",
        ":node",
        "//ir:addr",
        "//ir:register",
        "//type:typed_value",
        "//type:util",
    ],
    impl_deps = [
        "//misc:context",
    ],
)

cc_target(
    name = "function_literal",
    intf_deps = [
        ":declaration",
        ":identifier",
        ":literal",
        ":statements",
        "//core:fn_params",
        "//core:scope",
    ],
    impl_deps = [
        ":declaration",
        ":match_declaration",
        ":repeated_unop",
        ":terminal",
        "//backend:eval",
        "//misc:context",
        "//error:log",
        "//ir:compiled_fn",
        "//misc:module",
        "//type:function",
        "//type:tuple",
        "//type:type",
    ],
)

cc_lib(
    name = "hashtag",
    hdrs = ["hashtag.h"],
)

cc_target(
    name = "identifier",
    intf_deps = [":expression"],
    impl_deps = [
        ":declaration",
        ":function_literal",
        ":match_declaration",
        "//backend:eval",
        "//misc:context",
        "//error:log",
        "//ir:components",
        "//misc:module",
        "//core:scope",
        "//type:pointer",
        "//type:type",
    ],
)

cc_target(
    name = "import",
    intf_deps = [
        ":literal",
        "//misc:module",
    ],
    impl_deps = [
        ":overload_set",
        "//backend:eval",
        "//base:guarded",
        "//misc:context",
        "//type:primitive",
    ],
)

cc_target(
    name = "index",
    intf_deps = [":expression"],
    impl_deps = [
        "//backend:eval",
        "//ir:components",
        "//ir:str",
        "//core:arch",
        "//type:cast",
    ]
)

cc_target(
    name = "interface",
    intf_deps = [
        ":declaration",
        ":literal",
        "//core:scope",
    ],
    impl_deps = [
        "//error:log",
        "//misc:context",
        "//type:type",
    ],
)

cc_lib(
    name = "literal",
    hdrs = ["literal.h"],
    deps = [":expression"],
)

cc_target(
    name = "match_declaration",
    intf_deps = [":declaration"],
    impl_deps = [
        "//backend:eval",
        "//type:interface",
        "//type:primitive",
    ],
)

cc_lib(
    name = "node",
    hdrs = ["node.h"],
    deps = [
        "//ast_visitor:visitors",
        "//base:graph",
        "//base:util",
        "//frontend:text_span",
        "//ir:results",
    ],
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

cc_target(
    name = "repeated_unop",
    intf_deps = [
        ":comma_list",
        "//frontend:operators",
    ],
    impl_deps = [
        ":block_literal",
        ":call",
        ":function_literal",
        ":overload_set",
        "//backend:eval",
        "//core:fn_args",
        "//ir:compiled_fn",
        "//misc:context",
        "//core:scope",
    ],
)

cc_target(
    name = "scope_literal",
    intf_deps = [
        ":declaration",
        ":literal",
    ],
    impl_deps = [
        "//misc:context",
        "//error:log",
        "//core:scope",
        "//type:function",
        "//type:pointer",
    ],
)

cc_target(
    name = "scope_node",
    intf_deps = [
        ":block_node",
        ":expression",
        ":statements",
        "//core:fn_args",
        "//core:scope",
    ],
    impl_deps = [
        ":access",
        ":block_literal",
        ":block_node",
        ":function_literal",
        ":identifier",
        ":scope_literal",
        "//backend:eval",
        "//base:util",
        "//core:fn_args",
        "//ir:components",
        "//ir:compiled_fn",
        "//misc:context",
        "//core:scope",
        "//type:function",
        "//type:pointer",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_target(
    name = "statements",
    intf_deps = [":node"],
    impl_deps = [
        "//misc:context",
    ],
)

cc_target(
    name = "struct_literal",
    intf_deps = [
        ":comma_list",
        ":declaration",
        ":literal",
        "//core:scope",
    ],
    impl_deps = [
        ":declaration",
        "//backend:exec",
        "//base:untyped_buffer",
        "//ir:components",
        "//ir:compiled_fn",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
    ],
)

cc_target(
    name = "struct_type",
    intf_deps = [":literal"],
    impl_deps = [
        "//misc:context",
        "//type:type",
    ],
)

cc_target(
    name = "switch",
    intf_deps = [":literal"],
    impl_deps = [
        "//base:util",
        "//ir:components",
        "//ir:compiled_fn",
        "//ir:phi",
        "//type:cast",
        "//type:pointer",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_lib(
    name = "terminal",
    hdrs = ["terminal.h"],
    deps = [
        ":expression",
        "//ir:block",
        "//misc:context",
        "//misc:module"
    ],
)

cc_target(
    name = "unop",
    intf_deps = [
        ":expression",
        "//frontend:operators",
    ],
    impl_deps = [
        ":overload_set",
        ":terminal",
        "//backend:eval",
        "//core:fn_args",
        "//misc:context",
        "//ir:components",
        "//ir:compiled_fn",
    ],
)
