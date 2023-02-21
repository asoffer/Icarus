package(default_visibility = ["//visibility:private"])

cc_library(
    name = "module",
    visibility = ["//visibility:public"],
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":symbol",
        "//base:ptr_span",
        "//data_types:integer",
        "//semantic_analysis:instruction_set",
        "//semantic_analysis:type_system",
        "//serialization:foreign_symbol_map",
        "//serialization:function_table",
        "//serialization:module",
        "//serialization:module_map",
        "//serialization:read_only_data",
        "@asoffer_jasmin//jasmin:serialization",
    ],
)

cc_library(
    name = "module_name",
    visibility = ["//visibility:public"],
    hdrs = ["module_name.h"],
    deps = [],
)

cc_library(
    name = "resources",
    visibility = ["//visibility:public"],
    hdrs = ["resources.h"],
    srcs = ["resources.cc"],
    deps = [
        ":function_map",
        ":module",
        ":module_name",
        "//diagnostic/consumer",
        "//serialization:foreign_symbol_map",
        "//serialization:module_map",
        "//serialization:read_only_data",
        "@com_google_absl//absl/functional:any_invocable",
    ]
)

cc_library(
    name = "symbol",
    visibility = ["//visibility:public"],
    hdrs = ["symbol.h"],
    srcs = ["symbol.cc"],
    deps = [
        "//base:debug",
        "//data_types:fn",
        "//core/type_system:type",
        "//semantic_analysis:type_system",
    ],
)

cc_library(
    name = "function_map",
    visibility = ["//visibility:public"],
    hdrs = ["function_map.h"],
    deps = [
        "//serialization:function_index",
        "//serialization:module_index",
        "@com_google_absl//absl/container:flat_hash_map"
    ],
)
