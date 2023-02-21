package(default_visibility = ["//visibility:private"])

proto_library(
    name = "constants_proto",
    srcs = ["constants.proto"],
    deps = [],
)

cc_proto_library(
    name = "constants",
    visibility = ["//visibility:public"],
    deps = [":constants_proto"],
)

cc_proto_library(
    name = "function_index_proto",
    deps = ["//serialization/proto:function_index"],
)

cc_library(
    name = "function_index",
    hdrs = ["function_index.h"],
    srcs = ["function_index.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":function_index_proto",
        "//base:debug",
    ],
)

cc_proto_library(
    name = "function_table_proto",
    deps = ["//serialization/proto:function_table"],
)

cc_library(
    name = "function_table",
    hdrs = ["function_table.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":function_index",
        ":function_table_proto",
        ":module_index",
        "//base:debug",
        "//module:function_map",
        "@asoffer_jasmin//jasmin:serialization",
        "@com_google_absl//absl/container:btree",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_proto_library(
    name = "foreign_symbol_map_proto",
    deps = ["//serialization/proto:foreign_symbol_map"],
)


cc_library(
    name = "foreign_symbol_map",
    hdrs = ["foreign_symbol_map.h"],
    srcs = ["foreign_symbol_map.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":foreign_symbol_map_proto",
        ":function_index",
        "//base:debug",
        "//core/type_system:type",
        "@asoffer_nth//nth/container:flyweight_map",
    ],
)

proto_library(
    name = "module_proto",
    srcs = ["module.proto"],
    deps = [
        ":constants_proto",
        ":type_system_proto",
        "//serialization/proto:foreign_symbol_map",
        "//serialization/proto:function_index",
        "//serialization/proto:function_table",
        "//serialization/proto:module_map",
        "//serialization/proto:read_only_data",
    ],
)

cc_proto_library(
    name = "module",
    visibility = ["//visibility:public"],
    deps = [":module_proto"],
)

cc_library(
    name = "module_index",
    visibility = ["//visibility:public"],
    hdrs = ["module_index.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_format",
        "//base/extend:absl_hash",
        "//base/extend:compare",
    ],
)

cc_proto_library(
    name = "module_map_proto",
    visibility = ["//visibility:public"],
    deps = ["//serialization/proto:module_map"],
)

cc_library(
    name = "module_map",
    visibility = ["//visibility:public"],
    hdrs = ["module_map.h"],
    srcs = ["module_map.cc"],
    deps = [
        ":module_index",
        ":module_map_proto",
        ":unique_module_id",
        "@asoffer_nth//nth/container:flyweight_set",
    ],
)

cc_proto_library(
    name = "read_only_data_proto",
    deps = ["//serialization/proto:read_only_data"],
)

cc_library(
    name = "read_only_data",
    hdrs = ["read_only_data.h"],
    srcs = ["read_only_data.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":module_index",
        "//base:debug",
        "//serialization:read_only_data_proto",
        "@asoffer_nth//nth/container:flyweight_set",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

proto_library(
    name = "type_system_proto",
    visibility = ["//serialization:__subpackages__"],
    srcs = ["type_system.proto"],
    deps = [],
)

cc_proto_library(
    name = "type_system",
    visibility = ["//visibility:public"],
    deps = [":type_system_proto"],
)

cc_library(
    name = "unique_module_id",
    hdrs = ["unique_module_id.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//base:debug",
    ],
)
