package(default_visibility = ["//visibility:public"])

cc_library(
    name = "casting",
    hdrs = ["casting.h"],
    srcs = ["casting.cc"],
    deps = [
        "//semantic_analysis:type_system",
    ],
)

cc_test(
    name = "casting_test",
    srcs = ["casting_test.cc"],
    deps = [
        ":casting",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "diagnostics",
    hdrs = ["diagnostics.h"],
    srcs = ["diagnostics.cc"],
    deps = [
        "//ast",
        "//base:debug",
        "//semantic_analysis:context",
        "//semantic_analysis:type_system",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/strings",
    ]
)

cc_test(
    name = "diagnostics_test",
    srcs = ["diagnostics_test.cc"],
    deps = [
        ":diagnostics",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "verify",
    hdrs = ["verify.h"],
    srcs = [
        "access.cc",
        "array_literal.cc",
        "array_type.cc",
        "assignment.cc",
        "binary_operator.cc",
        "builtin.cc",
        "call.cc",
        "cast.cc",
        "comparison_operator.cc",
        "declaration.cc",
        "function_literal.cc",
        "function_type.cc",
        "identifier.cc",
        "if_stmt.cc",
        "import.cc",
        "short_function_literal.cc",
        "module.cc",
        "program_arguments.cc",
        "return_stmt.cc",
        "slice_type.cc",
        "unary_operator.cc",
        "terminal.cc",
    ],
    deps = [
        ":casting",
        ":diagnostics",
        "//ast",
        "//compiler:common_diagnostics",
        "//core:call",
        "//diagnostic/consumer",
        "//module",
        "//module:module_map",
        "//semantic_analysis/byte_code",
        "//semantic_analysis:context",
        "//semantic_analysis:instruction_set",
        "//semantic_analysis:type_system",
        "//semantic_analysis:task",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_nth//nth/meta:sequence",
        "@asoffer_nth//nth/meta:type",
        "@com_google_absl//absl/cleanup",
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
    name = "assignment_test",
    srcs = ["assignment_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "binary_operator_test",
    srcs = ["binary_operator_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "builtin_test",
    srcs = ["builtin_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "cast_test",
    srcs = ["cast_test.cc"],
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
    name = "comparison_operator_test",
    srcs = ["comparison_operator_test.cc"],
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
    name = "function_type_test",
    srcs = ["function_type_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "function_literal_test",
    srcs = ["function_literal_test.cc"],
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
    name = "if_stmt_test",
    srcs = ["if_stmt_test.cc"],
    deps = [
        ":verify",
        "//test:repl",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "import_test",
    srcs = ["import_test.cc"],
    deps = [
        ":verify",
        "//module:specified_module_map",
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
    name = "slice_type_test",
    srcs = ["slice_type_test.cc"],
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
