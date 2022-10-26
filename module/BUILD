package(default_visibility = ["//visibility:private"])

cc_library(
    name = "module",
    visibility = ["//visibility:public"],
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":module_cc_proto",
        "//semantic_analysis:instruction_set",
        "//semantic_analysis:type_system",
        "@asoffer_jasmin//jasmin:serialization",
    ],
)

proto_library(
    name = "module_proto",
    srcs = ["module.proto"],
    deps = [],
)

cc_proto_library(
    name = "module_cc_proto",
    deps = [":module_proto"],
)
