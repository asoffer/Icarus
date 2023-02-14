package(default_visibility = ["//visibility:private"])

cc_library(
    name = "module",
    visibility = ["//visibility:public"],
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":symbol",
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
