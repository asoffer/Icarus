package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_lib(
    name = "ast",
    deps = [
        ":access",
        ":array_literal",
        ":array_type",
        ":binop",
        ":block_literal",
        ":block_node",
        ":bound_constants",
        ":builtin_fn",
        ":call",
        ":cast",
        ":chainop",
        ":comma_list",
        ":declaration",
        ":dispatch",
        ":enum_literal",
        ":expression",
        ":fn_args",
        ":fn_params",
        ":function_literal",
        ":hashtag",
        ":hole",
        ":identifier",
        ":import",
        ":index",
        ":interface",
        ":match_declaration",
        ":node",
        ":node_lookup",
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
        ":bound_constants-impl",
        ":builtin_fn-impl",
        ":call-impl",
        ":cast-impl",
        ":chainop-impl",
        ":comma_list-impl",
        ":declaration-impl",
        ":dispatch-impl",
        ":enum_literal-impl",
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
    impl_deps = [
        ":hole",
        "//ir:cmd",
    ]
)

cc_target(
    name = "binop",
    intf_deps = [
        ":dispatch",
        ":literal",
        "//frontend:operators",
    ],
    impl_deps = [
        ":comma_list",
        ":fn_args",
        ":overload_set",
        "//backend:eval",
        "//misc:context",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
        "//type:variant",
    ],
)

cc_target(
    name = "block_literal",
    intf_deps = [":literal"],
    impl_deps = [
        "//misc:context",
        "//ir:val",
        "//misc:scope",
        "//type:function",
        "//type:primitive",
    ],
)

cc_target(
    name = "block_node",
    intf_deps = [
        ":expression",
        ":statements",
        "//misc:scope",
    ],
    impl_deps = ["//ir:val"],
)

cc_target(
    name = "bound_constants",
    intf_deps = ["//ir:val"],
    impl_deps = [":declaration"],
)

cc_target(
    name = "builtin_fn",
    intf_deps = [
        ":fn_args",
        ":literal",
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
        ":dispatch",
        ":literal",
        ":fn_args",
    ],
    impl_deps = [
        ":builtin_fn",
        ":fn_params",
        ":function_literal",
        ":struct_literal",
        ":unop",
        "//backend:eval",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//misc:scope",
        "//type:array",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
    ],
)

cc_target(
    name = "cast",
    intf_deps = [":expression"],
    impl_deps = [
        ":fn_args",
        ":overload_set",
        "//backend:eval",
        "//ir:cmd",
        "//misc:context",
        "//type:cast",
        "//type:tuple",
    ],
)

cc_target(
    name = "chainop",
    intf_deps = [
        ":dispatch",
        ":literal",
        "//frontend:operators",
    ],
    impl_deps = [
        ":fn_args",
        ":overload_set",
        "//misc:context",
        "//ir:func",
        "//ir:phi",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
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
        ":hole",
        "//backend:eval",
        "//error:inference_failure_reason",
        "//ir:func",
        "//misc:module",
        "//type:cast",
        "//type:typed_value",
    ],
)

cc_target(
    name = "dispatch",
    intf_deps = [
        ":bound_constants",
        ":fn_args",
        ":overload_set",
        "//base:expected",
    ],
    impl_deps = [
        ":call",
        ":builtin_fn",
        ":function_literal",
        ":match_declaration",
        "//backend:eval",
        "//misc:context",
        "//ir:cmd",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//misc:module",
        "//misc:scope",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
    ],
)

cc_target(
    name = "enum_literal",
    intf_deps = [
        ":declaration",
        ":literal",
        "//misc:scope",
    ],
    impl_deps = [
        ":hole",
        ":identifier",
        "//misc:context",
        "//ir:cmd",
    ],
)


cc_lib(
    name = "expression",
    hdrs = ["expression.h"],
    deps = [
        ":hashtag",
        ":node",
        "//misc:context",
        "//ir:addr",
        "//ir:register",
        "//type:typed_value",
        "//type:util",
    ]
)

cc_lib(
    name = "fn_args",
    hdrs = ["fn_args.h"],
    deps = [
    ],
)

cc_lib(
    name = "fn_params",
    hdrs = ["fn_params.h"],
    deps = [
    ],
)

cc_target(
    name = "function_literal",
    intf_deps = [
        ":bound_constants",
        ":declaration",
        ":dispatch",
        ":fn_params",
        ":identifier",
        ":literal",
        ":statements",
        "//ir:val",
        "//misc:scope",
    ],
    impl_deps = [
        ":bound_constants",
        ":declaration",
        ":match_declaration",
        ":repeated_unop",
        ":terminal",
        "//backend:eval",
        "//misc:context",
        "//error:log",
        "//ir:func",
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

cc_lib(
    name = "hole",
    hdrs = ["hole.h"],
    deps = [":literal"],
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
        "//ir:val",
        "//misc:module",
        "//misc:scope",
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
        "//misc:architecture",
        "//type:cast",
    ]
)

cc_target(
    name = "interface",
    intf_deps = [
        ":declaration",
        ":literal",
        "//misc:scope",
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
        "//base:graph",
        "//base:util",
        "//frontend:text_span",
        "//ir:results",
        "//misc:inference_state",
    ],
)

cc_lib(
    name = "node_lookup",
    hdrs = ["node_lookup.h"],
)

cc_target(
    name = "overload_set",
    intf_deps = [
        "//type:typed_value",
        "//type:function",
    ],
    impl_deps = [
        ":declaration",
        "//misc:context",
        "//misc:scope",
    ],
)

cc_target(
    name = "repeated_unop",
    intf_deps = [
        ":comma_list",
        ":dispatch",
        "//frontend:operators",
    ],
    impl_deps = [
        ":fn_args",
        ":function_literal",
        ":overload_set",
        "//ir:func",
        "//misc:context",
        "//misc:scope",
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
        "//ir:val",
        "//misc:scope",
        "//type:function",
        "//type:pointer",
    ],
)

cc_target(
    name = "scope_node",
    intf_deps = [
        ":block_node",
        ":expression",
        ":fn_args",
        ":statements",
        "//misc:scope",
    ],
    impl_deps = [
        ":access",
        ":block_literal",
        ":block_node",
        ":fn_args",
        ":function_literal",
        ":identifier",
        ":scope_literal",
        "//backend:eval",
        "//base:util",
        "//ir:components",
        "//ir:func",
        "//misc:context",
        "//misc:scope",
        "//type:function",
        "//type:pointer",
        "//type:type",
    ],
)

cc_target(
    name = "statements",
    intf_deps = [":node"],
    impl_deps = [
        "//misc:context",
        "//ir:val",
    ],
)

cc_target(
    name = "struct_literal",
    intf_deps = [
        ":comma_list",
        ":declaration",
        ":literal",
        "//misc:scope",
    ],
    impl_deps = [
        ":declaration",
        "//backend:exec",
        "//base:untyped_buffer",
        "//ir:components",
        "//ir:func",
        "//ir:val",
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
        "//ir:func",
        "//ir:phi",
        "//type:cast",
        "//type:pointer",
        "//type:type",
    ],
)

cc_lib(
    name = "terminal",
    hdrs = ["terminal.h"],
    deps = [
        ":expression",
        "//misc:context",
        "//misc:module"
    ],
)

cc_target(
    name = "unop",
    intf_deps = [
        ":dispatch",
        ":expression",
        "//frontend:operators",
    ],
    impl_deps = [
        ":fn_args",
        ":overload_set",
        ":terminal",
        "//backend:eval",
        "//misc:context",
        "//ir:components",
        "//ir:func",
    ],
)
