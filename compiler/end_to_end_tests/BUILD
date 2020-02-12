cc_test(
    name = "test",
    srcs = ["test.cc"],
    deps = [
        "//compiler",
        "//compiler:executable_module",
        "//frontend/source:string",
        "//interpretter:execute",
        "//test:failing_diagnostic_consumer",
        "@com_google_googletest//:gtest_main",
    ],
)
