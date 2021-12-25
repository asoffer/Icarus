package(default_visibility = ["//visibility:public"])

cc_library(
    name = "fuzz",
    hdrs = ["fuzz.h"],
    deps = [],
    testonly = True,
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    deps = [
        "//ast:ast",
        "//ast:expression",
        "//ast:overload_set",
        "//base:ptr_span",
        "//compiler",
        "//compiler:resources",
        "//compiler:work_graph",
        "//diagnostic/consumer:aborting",
        "//diagnostic/consumer:tracking",
        "//frontend/source:buffer",
        "//frontend:parse",
        "//module:module",
        "//module:mock_importer",
        "@com_google_absl//absl/types:span",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    testonly = True,
)

cc_library(
    name = "expected_value",
    hdrs = ["expected_value.h"],
    deps = [
        "//ir/value:result_buffer",
    ],
)

cc_library(
    name = "evaluation",
    hdrs = ["evaluation.h"],
    deps = [
        ":expected_value",
        ":module",
        "//type",
        "//type:typed_value",
        "@com_google_googletest//:gtest",
    ],
    testonly = True,
)

cc_library(
    name = "failing_diagnostic_consumer",
    hdrs = ["failing_diagnostic_consumer.h"],
    deps = [
        "//base:meta",
        "//diagnostic/consumer",
        "//diagnostic:message",
        "@com_google_googletest//:gtest",
    ],
)
