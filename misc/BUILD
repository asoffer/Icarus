package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

# Directory of things I haven't totally figured out how to organize yet.

cc_lib(
    name = "impl",
    deps = [
        ":context-impl",
        ":module-impl",
        ":scope-impl",
        ":import_graph-impl",
    ],
)

cc_target(
    name = "context",
    intf_deps = [
        ":module",
        "//ast:bound_constants",
        "//base:debug",
        "//ir:register",
    ],
    impl_deps = [
        "//ast:expression",
        "//ast:dispatch",
    ],
)

cc_target(
    name = "import_graph",
)

cc_lib(
    name = "inference_state",
    hdrs = ["inference_state.h"],
    deps = ["//base:debug"],
)

cc_target(
    name = "module",
    intf_deps = [
        ":scope",
        "//ast:fn_params",
        "//ast:bound_constants",
        "//ast:dispatch",
        "//ast:node_lookup",
        "//ast:statements",
        "//error:log",
    ],
    impl_deps = [
        ":import_graph",
        "//ast:declaration",
        "//ast:expression",
        "//ast:function_literal",
        "//ast:struct_literal",
        # "//backend:emit",
        "//backend:eval",
        "//base:guarded",
        "//frontend:source",
        "//ir:func",
        "//type:function",
    ],
)

cc_target(
    name = "scope",
    intf_deps = [
        "//base:util",
        "//type:typed_value",
    ],
    impl_deps = [
        ":context",
        ":module",
        "//ast:declaration",
        "//ast:identifier",
        "//ast:match_declaration",
        "//ir:cmd",
        "//type:function",
        "//type:pointer",
    ],
)
