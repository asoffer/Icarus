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
    srcs = ["module.cc"],
    deps = [
        "//ast:ast",
        "//ast:expression",
        "//base:ptr_span",
        "//compiler",
        "//compiler:resources",
        "//compiler:work_graph",
        "//diagnostic/consumer:tracking",
        "//frontend:parse",
        "//frontend:source_indexer",
        "//module:module",
        "//module:mock_importer",
        "//module:trivial_importer",
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
        ":module",
        ":expected_value",
        "//compiler:module",
        "//compiler:work_graph",
        "//frontend:parse",
        "//module:trivial_importer",
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

cc_library(
    name = "repl",
    testonly = True,
    hdrs = ["repl.h"],
    srcs = ["repl.cc"],
    deps = [
        "//ast:module",
        "//ast:expression",
        "//semantic_analysis:compiler_state",
        "//semantic_analysis:context",
        "//semantic_analysis/byte_code",
        "//diagnostic/consumer:tracking",
        "//frontend:parse",
        "//semantic_analysis/type_verification:verify",
        "@asoffer_jasmin//jasmin:execute",
        "@com_google_absl//absl/strings",
        "@com_google_googletest//:gtest",
    ],
)
