package(default_visibility = ["//serialization:__subpackages__"])

proto_library(
    name = "foreign_symbol_map",
    srcs = ["foreign_symbol_map.proto"],
    deps = ["//serialization:type_system_proto"],
)

proto_library(
    name = "function",
    srcs = ["function.proto"],
)

proto_library(
    name = "function_index",
    srcs = ["function_index.proto"],
)

proto_library(
    name = "function_table",
    srcs = ["function_table.proto"],
    deps = [
        ":function",
        ":function_index",
    ],
)

proto_library(
    name = "module_map",
    srcs = ["module_map.proto"],
)

proto_library(
    name = "read_only_data",
    srcs = ["read_only_data.proto"],
)
