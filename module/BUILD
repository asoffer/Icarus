package(default_visibility = ["//visibility:private"])

cc_library(
    name = "module",
    visibility = ["//visibility:public"],
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":global_index_map",
        ":global_module_map",
        ":symbol",
        ":type_system",
        "//base:ptr_span",
        "//data_types:integer",
        "//semantic_analysis:type_system",
        "//serialization:foreign_symbol_map",
        "//serialization:module",
        "//serialization:read_only_data",
        "//serialization:unique_type_table",
        "//vm:function",
        "//vm:serialization",
        "@asoffer_jasmin//jasmin:serialization",
        "@com_google_absl//absl/cleanup",
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
        ":global_function_map",
        ":global_index_map",
        ":global_module_map",
        ":module",
        ":module_name",
        "//diagnostic/consumer",
        "//serialization:foreign_symbol_map",
        "//serialization:read_only_data",
        "//serialization:unique_type_table",
        "@com_google_absl//absl/functional:any_invocable",
    ]
)

cc_library(
    name = "symbol",
    visibility = ["//visibility:public"],
    hdrs = ["symbol.h"],
    srcs = ["symbol.cc"],
    deps = [
        "//data_types:fn",
        "//core/type_system:type",
        "//semantic_analysis:type_system",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "global_function_map",
    visibility = ["//visibility:public"],
    hdrs = ["global_function_map.h"],
    srcs = ["global_function_map.cc"],
    deps = [
        "//serialization:function_index",
        "//serialization:module_index",
        "//serialization:unique_module_id",
        "@com_google_absl//absl/container:flat_hash_map"
    ],
)


cc_library(
    name = "global_index_map",
    visibility = ["//visibility:public"],
    hdrs = ["global_index_map.h"],
    srcs = ["global_index_map.cc"],
    deps = [
        "//serialization:function_index",
        "//serialization:module_index",
        "@asoffer_nth//nth/debug",
        "@com_google_absl//absl/container:flat_hash_map"
    ],
)

cc_library(
    name = "global_module_map",
    visibility = ["//visibility:public"],
    hdrs = ["global_module_map.h"],
    srcs = ["global_module_map.cc"],
    deps = [
        "//serialization:module_index",
        "//serialization:module_map_proto",
        "//serialization:unique_module_id",
        "@com_google_absl//absl/container:flat_hash_map",
        "@asoffer_nth//nth/container:flyweight_set",
    ],
)


cc_library(
    name = "type_system",
    visibility = ["//visibility:public"],
    hdrs = ["type_system.h"],
    srcs = ["type_system.cc"],
    deps = [
        "//semantic_analysis:type_system",
        "//serialization:type_system",
        "//serialization:unique_type_table",
    ],
)
