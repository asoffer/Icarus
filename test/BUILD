package(default_visibility = ["//visibility:public"])

cc_library(
    name = "fuzz",
    hdrs = ["fuzz.h"],
    deps = [],
    testonly = True,
)

cc_library(
    name = "failing_diagnostic_consumer",
    hdrs = ["failing_diagnostic_consumer.h"],
    deps = [
        "//diagnostic/consumer",
        "//diagnostic:message",
        "@asoffer_nth//nth/meta:type",
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
        "//module",
        "//module:resources",
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
