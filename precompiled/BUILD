package(default_visibility = ["//visibility:public"])

proto_library(
    name = "module_proto",
    srcs = ["module.proto"],
    deps = [
        ":value",
        "//ir:subroutine_proto",
    ],
)

cc_proto_library(
    name = "module_cc_proto",
    deps = [":module_proto"],
)

proto_library(
    name = "value",
    srcs = ["value.proto"],
)

cc_proto_library(
    name = "value_cc_proto",
    deps = [":value"],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":module_cc_proto",
        ":serialize",
        "//base:file",
        "//base:flyweight_map",
        "//module",
        "//module:map",
        "//module:shared_context",
        "//type",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:generic",
        "//type:generic_function",
        "//type:opaque",
        "//type:pointer",
        "//type:primitive",
        "//type:slice",
        "//type:struct",
        "//type:system",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "serialize",
    hdrs = ["serialize.h"],
    srcs = ["serialize.cc"],
    deps = [
        ":module_cc_proto",
        ":value_cc_proto",
        "//base:flyweight_map",
        "//ir/value:result_buffer",
        "//module:shared_context",
        "//module:table",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:generic",
        "//type:generic_function",
        "//type:opaque",
        "//type:pointer",
        "//type:primitive",
        "//type:slice",
        "//type:struct",
        "//type:system",
        "//type:variable",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "serialize_test",
    srcs = ["serialize_test.cc"],
    deps = [
        ":serialize",
        "@com_google_googletest//:gtest_main",
    ],
)
