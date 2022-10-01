package(default_visibility = ["//visibility:public"])

cc_library(
    name = "verify",
    hdrs = ["verify.h"],
    srcs = [
        "array_literal.cc",
        "array_type.cc",
        "call.cc",
        "declaration.cc",
        "identifier.cc",
        "short_function_literal.cc",
        "program_arguments.cc",
        "unary_operator.cc",
        "terminal.cc",
    ],
    deps = [
        "//ast",
        "//base:meta",
        "//compiler:common_diagnostics",
        "//diagnostic/consumer",
        "//semantic_analysis/byte_code",
        "//semantic_analysis/byte_code:instruction_set",
        "//semantic_analysis:context",
        "//semantic_analysis:type_system",
        "//semantic_analysis:task",
        "@asoffer_jasmin//jasmin:execute",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "array_literal_test",
    srcs = ["array_literal_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "array_type_test",
    srcs = ["array_type_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "call_test",
    srcs = ["call_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "declaration_test",
    srcs = ["declaration_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "identifier_test",
    srcs = ["identifier_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "short_function_literal_test",
    srcs = ["short_function_literal_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "program_arguments_test",
    srcs = ["program_arguments_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "unary_operator_test",
    srcs = ["unary_operator_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "terminal_test",
    srcs = ["terminal_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)
