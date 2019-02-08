package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

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
        ":call",
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
        ":call-impl",
        ":chainop-impl",
        ":comma_list-impl",
        ":declaration-impl",
        ":dispatch-impl",
        ":function_literal-impl",
        ":identifier-impl",
        ":overload_set-impl",
        ":scope_node-impl",
        ":statements-impl",
        ":struct_literal-impl",
    ],
)

cc_lib(
    name = "access",
    hdrs = ["access.h"],
    deps = [":expression"],
)

cc_lib(
    name = "access-impl",
    srcs = ["access.cc"],
    deps = [
        ":access",
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

cc_lib(
    name = "array_literal",
    hdrs = ["array_literal.h"],
    deps = [":comma_list"],
)

cc_lib(
    name = "array_literal-impl",
    srcs = ["array_literal.cc"],
    deps = [
        ":array_literal",
        "//misc:context",
        "//error:log",
        "//ir:cmd",
        "//type:array",
        "//type:cast",
        "//type:pointer",
    ],
)

cc_lib(
    name = "array_type",
    hdrs = ["array_type.h"],
    deps = [":expression"],
)

cc_lib(
    name = "array_type-impl",
    srcs = ["array_type.cc"],
    deps = [
        ":array_type",
        ":hole",
        "//ir:cmd",
    ]
)

cc_lib(
    name = "binop",
    hdrs = ["binop.h"],
    deps = [
        ":comma_list",
        ":dispatch",
        ":expression",
        ":fn_args",
        ":overload_set",
        "//backend:eval",
        "//base:check",
        "//frontend:operators",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//ir:val",
        "//misc:context",
        "//type:array",
        "//type:cast",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
        "//type:variant",
    ],
)

cc_lib(
    name = "binop-impl",
    srcs = ["binop.cc"],
    deps = [
        ":binop",
        ":comma_list",
        ":fn_args",
        ":overload_set",
        "//backend:eval",
        "//base:check",
        "//misc:context",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//type:array",
        "//type:cast",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
        "//type:variant",
    ],
)

cc_lib(
    name = "block_literal",
    hdrs = ["block_literal.h"],
    deps = [":expression"],
)

cc_lib(
    name = "block_literal-impl",
    srcs = ["block_literal.cc"],
    deps = [
        ":block_literal",
        "//misc:context",
        "//ir:val",
        "//misc:scope",
        "//type:function",
        "//type:primitive",
    ],
)

cc_lib(
    name = "block_node",
    hdrs = ["block_node.h"],
    deps = [
        "//ast:expression",
        "//ast:statements",
        "//misc:scope",
    ],
)

cc_lib(
    name = "block_node-impl",
    srcs = ["block_node.cc"],
    deps = [
        ":block_node",
        "//ir:val",
    ],
)

cc_lib(
    name = "bound_constants",
    hdrs = ["bound_constants.h"],
    deps = [
        "//base/container:map",
        "//ir:val",
    ],
)

cc_lib(
    name = "call",
    hdrs = ["call.h"],
    deps = [
        ":dispatch",
        ":expression",
        ":fn_args",
    ],
)

cc_lib(
    name = "call-impl",
    srcs = ["call.cc"],
    deps = [
        ":call",
        ":fn_params",
        ":function_literal",
        ":terminal",
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

cc_lib(
    name = "chainop",
    hdrs = ["chainop.h"],
    deps = [
        ":dispatch",
        ":expression",
        "//base/container:vector",
        "//frontend:operators",
    ],
)

cc_lib(
    name = "chainop-impl",
    srcs = ["chainop.cc"],
    deps = [
        ":chainop",
        ":fn_args",
        ":overload_set",
        "//base:check",
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

cc_lib(
    name = "comma_list",
    hdrs = ["comma_list.h"],
    deps = [":expression"],
)

cc_lib(
    name = "comma_list-impl",
    srcs = ["comma_list.cc"],
    deps = [
        ":comma_list",
        "//misc:context",
        "//ir:cmd",
        "//type:tuple",
    ],
)

cc_lib(
    name = "declaration",
    hdrs = ["declaration.h"],
    deps = [
        ":expression",
        "//ir:register",
    ],
)

cc_lib(
    name = "declaration-impl",
    srcs = ["declaration.cc"],
    deps = [
        ":declaration",
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

cc_lib(
    name = "dispatch",
    hdrs = ["dispatch.h"],
    deps = [
        ":bound_constants",
        ":fn_args",
        ":overload_set",
        "//base/container:map",
        "//base/container:unordered_map",
        "//base:expected",
    ],
)

cc_lib(
    name = "dispatch-impl",
    srcs = ["dispatch.cc"],
    deps = [
        ":call",
        ":function_literal",
        ":match_declaration",
        ":terminal",
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

cc_lib(
    name = "enum_literal",
    hdrs = ["enum_literal.h"],
    srcs = ["enum_literal.cc"],
    deps = [
        ":declaration",
        ":hole",
        ":identifier",
        "//base/container:vector",
        "//misc:context",
        "//misc:scope",
        "//ir:cmd",
        "//ir:val",
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
        "//base/container:unordered_map",
        "//base/container:vector",
    ],
)

cc_lib(
    name = "fn_params",
    hdrs = ["fn_params.h"],
    deps = [
        "//base/container:unordered_map",
        "//base/container:vector",
    ],
)

cc_lib(
    name = "function_literal",
    hdrs = ["function_literal.h"],
    deps = [
        ":bound_constants",
        ":declaration",
        ":dispatch",
        ":expression",
        ":fn_params",
        ":identifier",
        ":statements",
        "//base/container:map",
        "//base/container:vector",
        "//ir:val",
        "//misc:scope",
    ],
)

cc_lib(
    name = "function_literal-impl",
    srcs = ["function_literal.cc"],
    deps = [
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
)

cc_lib(
    name = "identifier",
    hdrs = ["identifier.h"],
    deps = [":expression"],
)

cc_lib(
    name = "identifier-impl",
    srcs = ["identifier.cc"],
    deps = [
        ":declaration",
        ":function_literal",
        ":identifier",
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

cc_lib(
    name = "import",
    hdrs = ["import.h"],
    srcs = ["import.cc"],
    deps = [
        "//ast:expression",
        "//ast:overload_set",
        "//backend:eval",
        "//base:guarded",
        "//misc:context",
        "//misc:module",
        "//ir:val",
        "//type:primitive",
    ],
)

cc_lib(
    name = "interface",
    hdrs = ["interface.h"],
    srcs = ["interface.cc"],
    deps = [
        ":declaration",
        ":expression",
        "//error:log",
        "//ir:val",
        "//misc:context",
        "//misc:scope",
        "//type:type",
    ],
)

cc_lib(
    name = "match_declaration",
    hdrs = ["match_declaration.h"],
    srcs = ["match_declaration.cc"],
    deps = [
        ":declaration",
        "//backend:eval",
        "//type:interface",
        "//type:primitive",
    ],
)

cc_lib(
    name = "node",
    hdrs = ["node.h"],
    deps = [
        "//base/container:unordered_map",
        "//base/container:vector",
        "//base:untyped_buffer",
        "//base:util",
        "//frontend:text_span",
    ],
)

cc_lib(
    name = "node_lookup",
    hdrs = ["node_lookup.h"],
    deps = ["//base/container:unordered_map"],
)

cc_lib(
    name = "overload_set",
    hdrs = ["overload_set.h"],
    deps = [
        "//base/container:vector",
        "//type:typed_value",
        "//type:function",
    ],
)

cc_lib(
    name = "overload_set-impl",
    srcs = ["overload_set.cc"],
    deps = [
        ":overload_set",
        ":declaration",
        "//misc:context",
        "//misc:scope",
    ],
)

cc_lib(
    name = "repeated_unop",
    hdrs = ["repeated_unop.h"],
    srcs = ["repeated_unop.cc"],
    deps = [
        ":comma_list",
        ":dispatch",
        ":fn_args",
        ":function_literal",
        ":overload_set",
        "//frontend:operators",
        "//misc:context",
        "//ir:func",
        "//misc:scope",
        "//type:util",
    ],
)

cc_lib(
    name = "scope_literal",
    hdrs = ["scope_literal.h"],
    srcs = ["scope_literal.cc"],
    deps = [
        ":expression",
        ":declaration",
        "//misc:context",
        "//error:log",
        "//ir:val",
        "//misc:scope",
        "//type:function",
        "//type:pointer",
    ],
)

cc_lib(
    name = "scope_node",
    hdrs = ["scope_node.h"],
    deps = [
        ":block_node",
        ":expression",
        ":fn_args",
        ":statements",
        "//base/container:unordered_map",
        "//misc:scope",
    ],
)

cc_lib(
    name = "scope_node-impl",
    srcs = ["scope_node.cc"],
    deps = [
        ":access",
        ":block_literal",
        ":block_node",
        ":fn_args",
        ":function_literal",
        ":identifier",
        ":scope_node",
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

cc_lib(
    name = "statements",
    hdrs = ["statements.h"],
    deps = [
        ":node",
        "//base/container:vector",
        "//ir:val",
    ],
)

cc_lib(
    name = "statements-impl",
    srcs = ["statements.cc"],
    deps = [
        ":statements",
        "//misc:context",
        "//ir:val",
    ],
)

cc_lib(
    name = "struct_literal",
    hdrs = ["struct_literal.h"],
    deps = [
        ":comma_list",
        ":declaration",
        ":expression",
        "//misc:scope",
    ],
)

cc_lib(
    name = "struct_literal-impl",
    srcs = ["struct_literal.cc"],
    deps = [
        ":declaration",
        ":struct_literal",
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

cc_lib(
    name = "struct_type",
    hdrs = ["struct_type.h"],
    srcs = ["struct_type.cc"],
    deps = [
        ":expression",
        "//misc:context",
        "//type:type",
    ],
)

cc_lib(
    name = "switch",
    hdrs = ["switch.h"],
    srcs = ["switch.cc"],
    deps = [
        ":expression",
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
    srcs = ["terminal.cc"],
    deps = [
        ":expression",
        "//ir:val",
        "//misc:context",
        "//misc:module",
    ],
)

cc_lib(
    name = "unop",
    hdrs = ["unop.h"],
    srcs = ["unop.cc"],
    deps =[
        ":fn_args",
        ":overload_set",
        ":dispatch",
        ":terminal",
        "//backend:eval",
        "//base:check",
        "//frontend:operators",
        "//misc:context",
        "//ir:components",
        "//ir:func",
        "//type:util",
    ],
)
