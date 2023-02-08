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
        ":type_system_proto",
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
    ],
)


cc_library(
    name = "module_map",
    visibility = ["//visibility:public"],
    hdrs = ["module_map.h"],
    srcs = ["module_map.cc"],
    deps = [
        ":module_index",
        "//base:debug",
        "@asoffer_nth//nth/container:flyweight_set",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "module_map_test",
    srcs = ["module_map_test.cc"],
    deps = [
        ":module_map",
        "@com_google_googletest//:gtest_main",
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

cc_test(
    name = "read_only_data_test",
    srcs = ["read_only_data_test.cc"],
    deps = [
        ":read_only_data",
        "@com_google_googletest//:gtest_main",
    ],
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
