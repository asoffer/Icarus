package(default_visibility = ["//visibility:public"])

proto_library(
    name = "module_proto",
    srcs = ["module.proto"],
    deps = [":value"],
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
        "//base:file",
        "//base:flyweight_map",
        "//module",
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
