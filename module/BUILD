package(default_visibility = ["//visibility:public"])

cc_library(
    name = "module",
    hdrs = ["module.h"],
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
    name = "builtin",
    hdrs = ["builtin.h"],
    deps = [
        ":module",
        "@com_google_absl//absl/types:span",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "builtin_test",
    srcs = ["builtin_test.cc"],
    deps = [
        ":builtin",
        "//type:primitive",
        "//type:qual_type",
        "@com_google_googletest//:gtest_main",
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
        "//ir/value:module_id",
        "@com_google_googletest//:gtest",
    ],
    testonly = True,
)

cc_library(
    name = "precompiled",
    hdrs = ["precompiled.h"],
    srcs = ["precompiled.cc"],
    deps = [
        ":module",
        ":reader",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "reader",
    hdrs = ["reader.h"],
    deps = [
        "//base:debug",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "writer",
    hdrs = ["writer.h"],
    deps = [
        "//base:debug",
        "@com_google_absl//absl/types:span",
    ],
)
