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
        ":util",
        "//ast:ast",
        "//ast:expression",
        "//ast:overload_set",
        "//base:ptr_span",
        "//compiler",
        "//diagnostic/consumer:trivial",
        "//frontend/source:range",
        "//module:module",
    ],
    testonly = True,
)

cc_library(
    name = "util",
    hdrs = ["util.h"],
    deps = [
        "//diagnostic/consumer:aborting",
        "//frontend:parse",
        "//frontend/source:string",
        "//module:module",
        "//type:function",
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
