package(default_visibility = ["//visibility:public"])

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:module",
        "//ast:scope",
        "//base:cast",
        "//base:debug",
        "//base:macros",
        "//type:qual_type",
        "@com_google_absl//absl/types:span",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)

cc_library(
    name = "importer",
    hdrs = ["importer.h"],
    srcs = ["importer.cc"],
    deps = [
        ":module",
        "//ir/value:module_id",
        "@com_google_absl//absl/types:span",
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
