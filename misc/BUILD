package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

# Directory of things I haven't totally figured out how to organize yet.

cc_lib(
    name = "impl",
    deps = [
        "architecture-impl",
        "context-impl",
        "module-impl",
        "scope-impl",
    ],
)

cc_lib(
    name = "architecture",
    hdrs = ["architecture.h"],
    deps = [
        "//base:types",
        "//ir:addr",
        "//ir:register",
    ],
)

cc_lib(
    name = "architecture-impl",
    srcs = ["architecture.cc"],
    deps = [
        ":architecture",
        ":context",
        "//ir:cmd",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:generic_struct",
        "//type:interface",
        "//type:opaque",
        "//type:pointer",
        "//type:primitive",
        "//type:struct",
        "//type:tuple",
        "//type:variant",
    ],
)

cc_lib(
    name = "context",
    hdrs = ["context.h"],
    deps = [
        ":module",
        "//ast:bound_constants",
        "//base/container:unordered_map",
        "//base/container:vector",
        "//base:debug",
        "//error:log",
        "//ir:register",
    ],
)  

cc_lib(
    name = "context-impl",
    srcs = ["context.cc"],
    deps = [
        ":context",
        "//ast:expression",
        "//ast:dispatch",
    ],
)

cc_lib(
    name = "import_graph",
    hdrs = ["import_graph.h"],
    srcs = ["import_graph.cc"],
    deps = ["//base/container:unordered_map"],
)

cc_lib(
    name = "module",
    hdrs = ["module.h"],
    deps = [
        ":scope",
        "//ast:fn_params",
        "//ast:bound_constants",
        "//ast:dispatch",
        "//ast:node_lookup",
        "//ast:statements",
        "//base/container:unordered_map",
        "//base/container:vector",
    ],
)

cc_lib(
    name = "module-impl",
    srcs = ["module.cc"],
    deps = [
        ":module",
        "//ast:declaration",
        "//ast:expression",
        "//ast:function_literal",
        "//ast:struct_literal",
        # "//backend:emit",
        "//backend:eval",
        "//base:guarded",
        "//frontend:source",
        "//misc:import_graph",
        "//ir:func",
        "//type:function",
    ],
)

cc_lib(
    name = "scope",
    hdrs = ["scope.h"],
    deps = [
        "//base/container:unordered_map",
        "//base/container:vector",
        "//base:util",
        "//type:typed_value",
    ],
)


cc_lib(
    name = "scope-impl",
    srcs = ["scope.cc"],
    deps = [
        ":context",
        ":module",
        ":scope",
        "//ast:declaration",
        "//ast:identifier",
        "//ast:match_declaration",
        "//ir:cmd",
        "//type:function",
        "//type:pointer",
    ],
)
