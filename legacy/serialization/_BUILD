package(default_visibility = ["//visibility:public"])

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

cc_proto_library(
    name = "function_proto",
    deps = ["//serialization/proto:function"],
    visibility = ["//visibility:public"],
)

cc_proto_library(
    name = "function_table_proto",
    visibility = ["//visibility:public"],
    deps = ["//serialization/proto:function_table"],
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
        "//core/type_system:type",
        "//module:function_id",
        "@asoffer_nth//nth/container:flyweight_map",
        "@asoffer_nth//nth/debug",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

proto_library(
    name = "module_proto",
    srcs = ["module.proto"],
    deps = [
        ":constants_proto",
        ":type_system_proto",
        "//serialization/proto:foreign_symbol_map",
        "//serialization/proto:function",
        "//serialization/proto:function_index",
        "//serialization/proto:function_table",
        "//serialization/proto:read_only_data",
    ],
)

cc_proto_library(
    name = "module",
    visibility = ["//visibility:public"],
    deps = [":module_proto"],
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
        "//serialization:read_only_data_proto",
        "@asoffer_nth//nth/container:flyweight_set",
        "@asoffer_nth//nth/debug",
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
    name = "unique_type_table",
    hdrs = ["unique_type_table.h"],
    srcs = ["unique_type_table.cc"],
    visibility = ["//visibility:public"],
    deps = [
        ":type_system",
        "//module:unique_id",
        "@asoffer_nth//nth/debug",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)
