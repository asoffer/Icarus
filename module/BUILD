package(default_visibility = ["//visibility:private"])

cc_library(
    name = "function_id",
    hdrs = ["function_id.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":unique_id",
        "//serialization:function_index_proto",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/strings:interpolate",
    ],
)

cc_library(
    name = "module",
    visibility = ["//visibility:public"],
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
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
    name = "module_map",
    visibility = ["//visibility:public"],
    hdrs = ["module_map.h"],
    srcs = ["module_map.cc"],
    deps = [
        ":module",
        ":module_name",
        ":name_resolver",
        ":unique_id",
        "//base:file",
        "//serialization:module",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/io:file_path",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "module_name",
    visibility = ["//visibility:public"],
    hdrs = ["module_name.h"],
    deps = [],
)

cc_library(
    name = "name_resolver",
    hdrs = ["name_resolver.h"],
    srcs = ["name_resolver.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":module_name",
        ":unique_id",
        "@asoffer_nth//nth/debug",
        "@com_google_absl//absl/container:flat_hash_map"
    ],
)

cc_library(
    name = "resources",
    visibility = ["//visibility:public"],
    hdrs = ["resources.h"],
    srcs = ["resources.cc"],
    deps = [
        ":global_function_map",
        ":module",
        ":module_map",
        ":module_name",
        "//diagnostic/consumer",
        "//serialization:foreign_symbol_map",
        "//serialization:read_only_data",
        "//serialization:unique_type_table",
        "@asoffer_nth//nth/debug",
        "@com_google_absl//absl/functional:any_invocable",
        "@com_google_absl//absl/functional:function_ref",
    ]
)

cc_library(
    name = "symbol",
    visibility = ["//visibility:public"],
    hdrs = ["symbol.h"],
    srcs = ["symbol.cc"],
    deps = [
        ":function_id",
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
        ":function_id",
        ":unique_id",
        "@com_google_absl//absl/container:flat_hash_map"
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

cc_library(
    name = "unique_id",
    hdrs = ["unique_id.h"],
    srcs = ["unique_id.cc"],
    visibility = ["//visibility:public"],
    deps = [
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/utility:no_destructor",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/synchronization",
    ],
)

cc_test(
    name = "unique_id_test",
    srcs = ["unique_id_test.cc"],
    deps = [
        ":unique_id",
        "@asoffer_nth//nth/test:main",
    ],
)

cc_library(
    name = "serialize",
    visibility = ["//visibility:public"],
    hdrs = ["serialize.h"],
    srcs = ["serialize.cc"],
    deps = [
        ":module",
        ":module_map",
        ":symbol",
        ":type_system",
        "//base:ptr_span",
        "//core:serialize",
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

