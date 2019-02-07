package(default_visibility = ["//visibility:public"])
# Directory of things I haven't totally figured out how to organize yet.

cc_library(
    name = "architecture",
    hdrs = ["architecture.h"],
    deps = [
        "//base:types",
        "//ir:addr",
        "//ir:register",
    ],
)

cc_library(
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

cc_library(
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

cc_library(
    name = "context-impl",
    srcs = ["context.cc"],
    deps = [
        ":context",
        "//ast:expression",
        "//ast:dispatch",
    ],
)

cc_library(
    name = "import_graph",
    hdrs = ["import_graph.h"],
    srcs = ["import_graph.cc"],
    deps = ["//base/container:unordered_map"],
)

cc_library(
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

cc_library(
    name = "scope",
    hdrs = ["scope.h"],
    deps = [
        "//base/container:unordered_map",
        "//base/container:vector",
        "//base:util",
        "//type:typed_value",
    ],
)
