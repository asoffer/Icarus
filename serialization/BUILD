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

proto_library(
    name = "module_proto",
    srcs = ["module.proto"],
    deps = [
        ":constants_proto",
        ":read_only_data_proto",
        ":type_system_proto",
    ],
)

cc_proto_library(
    name = "module",
    visibility = ["//visibility:public"],
    deps = [":module_proto"],
)

proto_library(
    name = "read_only_data_proto",
    srcs = ["read_only_data.proto"],
    deps = [],
)

cc_proto_library(
    name = "read_only_data",
    visibility = ["//visibility:public"],
    deps = [":read_only_data_proto"],
)

proto_library(
    name = "type_system_proto",
    srcs = ["type_system.proto"],
    deps = [],
)

cc_proto_library(
    name = "type_system",
    visibility = ["//visibility:public"],
    deps = [":type_system_proto"],
)
