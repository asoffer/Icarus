package(default_visibility = ["//visibility:public"])

cc_library(
    name = "lex",
    hdrs = ["lex.cc"],
)

cc_library(
    name = "numbers",
    hdrs = ["numbers.h"],
    srcs = ["numbers.cc"],
    deps = [
        "//base:types",
        "//base:debug",
    ],
)

cc_library(
    name = "operators",
    hdrs = ["operators.h"],
    textual_hdrs = ["operators.xmacro.h"],
)

cc_library(
    name = "parse",
    srcs = ["parse.cc"],
    deps = [
        ":tagged_node",
        ":token",
        "//ast:access",
        "//ast:array_literal",
        "//ast:array_type",
        "//ast:binop",
        "//ast:block_literal",
        "//ast:block_node",
        "//ast:bound_constants",
        "//ast:call",
        "//ast:chainop",
        "//ast:comma_list",
        "//ast:declaration",
        "//ast:enum_literal",
        "//ast:expression",
        "//ast:fn_args",
        "//ast:fn_params",
        "//ast:function_literal",
        "//ast:hashtag",
        "//ast:hole",
        "//ast:identifier",
        "//ast:import",
        "//ast:interface",
        "//ast:match_declaration",
        "//ast:node",
        "//ast:node_lookup",
        "//ast:repeated_unop",
        "//ast:scope_literal",
        "//ast:scope_node",
        "//ast:statements",
        "//ast:struct_literal",
        "//ast:struct_type",
        "//ast:switch",
        "//ast:terminal",
        "//ast:unop",
    ],
)

cc_library(
    name = "source",
    hdrs = ["source.h"],
    srcs = ["source.cc"],
    deps = ["//base/container:vector"],
)

cc_library(
    name = "tagged_node",
    hdrs = ["tagged_node.h"],
    srcs = ["tagged_node.h"],
)

cc_library(
    name = "text_span",
    hdrs = ["text_span.h"],
    srcs = ["text_span.cc"],
    deps = [
        ":source",
        "//base:debug",
        "//base:interval",
        "//base:types",
    ],
)

cc_library(
    name = "token",
    hdrs = ["token.h"],
    deps = [
        ":operators",
        "//ast:node",
    ]
)
