package(default_visibility = ["//visibility:private"])

cc_library(
    name = "bazel_module_map",
    visibility = ["//visibility:public"],
    hdrs = ["bazel_module_map.h"],
    srcs = ["bazel_module_map.cc"],
    deps = [
        ":module_map",
        ":specified_module_map",
        "//base:file",
        "@com_google_absl//absl/strings",
    ]
)

cc_library(
    name = "module",
    visibility = ["//visibility:public"],
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        "//data_types:integer",
        "//semantic_analysis:instruction_set",
        "//semantic_analysis:type_system",
        "//serialization:module",
        "@asoffer_jasmin//jasmin:serialization",
    ],
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
        ":module",
        ":module_index",
        "//base:debug",
        "@asoffer_nth//nth/container:flyweight_map",
    ],
)

cc_library(
    name = "specified_module_map",
    visibility = ["//visibility:public"],
    hdrs = ["specified_module_map.h"],
    deps = [
        ":module_map",
        "@com_google_absl//absl/container:flat_hash_map",
    ]
)
