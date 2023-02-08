package(default_visibility = ["//visibility:private"])

cc_library(
    name = "bazel_name_resolver",
    visibility = ["//visibility:public"],
    hdrs = ["bazel_name_resolver.h"],
    srcs = ["bazel_name_resolver.cc"],
    deps = [
        ":module_name",
        "//base:debug",
        "//base:file",
        "//serialization:module_map",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/functional:any_invocable",
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
    name = "module_name",
    visibility = ["//visibility:public"],
    hdrs = ["module_name.h"],
    deps = [],
)

cc_library(
    name = "resources",
    visibility = ["//visibility:public"],
    hdrs = ["resources.h"],
    srcs = ["resources.cc"],
    deps = [
        ":module",
        ":module_name",
        "//serialization:module_map",
        "//serialization:read_only_data",
        "@com_google_absl//absl/functional:any_invocable",
    ]
)
