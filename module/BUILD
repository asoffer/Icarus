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

cc_test(
    name = "module_test",
    srcs = ["module_test.cc"],
    deps = [
        ":module",
        ":mock_module",
        "//type:qual_type",
        "@com_google_absl//absl/container:node_hash_map",
        "@com_google_googletest//:gtest_main",
    ],
)


cc_library(
    name = "mock_module",
    hdrs = ["mock_module.h"],
    deps = [
        ":module",
        "@com_google_googletest//:gtest",
    ],
    testonly = True,
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
    name = "trivial_importer",
    hdrs = ["trivial_importer.h"],
    deps = [
        ":importer",
        ":shared_context",
        "//ir/value:module_id",
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
        "//base:flyweight_map",
        "//type",
        "//type:system",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "reader",
    hdrs = ["reader.h"],
    srcs = ["reader.cc"],
    deps = [
        ":shared_context",
        "//base:debug",
        "//base:flyweight_map",
        "//type:primitive",
        "//type:serialize",
        "//type:system",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "shared_context",
    hdrs = ["shared_context.h"],
    deps = [
        ":module",
        ":table",
        "//base:flyweight_map",
        "//ir/value:foreign_fn",
        "//type:function",
    ],
)

cc_library(
    name = "table",
    hdrs = ["table.h"],
    deps = [
        ":builtin",
        ":module",
        "//ir/value:module_id",
    ],
)

cc_test(
    name = "table_test",
    srcs = ["table_test.cc"],
    deps = [
        ":builtin",
        ":mock_module",
        ":module",
        ":table",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "writer",
    hdrs = ["writer.h"],
    deps = [
        "//base:debug",
        "//type:primitive",
        "//type:serialize",
        "//type:system",
        "@com_google_absl//absl/types:span",
    ],
)
