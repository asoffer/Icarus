package(default_visibility = ["//visibility:public"])

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":importer",
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast/scope:module",
        "//base:cast",
        "//base:debug",
        "//base:expected",
        "//base:graph",
        "//base:guarded",
        "//base:macros",
        "//base:ptr_span",
        "//diagnostic/consumer",
        "//frontend:parse",
        "@com_google_absl//absl/algorithm:container",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/container:node_hash_map",
        "@com_google_absl//absl/synchronization",
    ],
)

cc_library(
    name = "importer",
    hdrs = ["importer.h"],
    srcs = ["importer.cc"],
    deps = [
        "//diagnostic/consumer:streaming",
        "//frontend/source",
        "//frontend/source:file",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//frontend:parse",
        "//ir/value:module_id",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "mock_importer",
    hdrs = ["mock_importer.h"],
    deps = [
        ":importer",
        "//frontend/source:file_name",
        "//ir/value:module_id",
        "@com_google_googletest//:gtest",
    ],
    testonly = True,
)
