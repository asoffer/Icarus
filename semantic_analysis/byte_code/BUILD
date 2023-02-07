package(default_visibility = ["//visibility:public"])

cc_library(
    name = "byte_code",
    hdrs = ["byte_code.h"],
    srcs = ["byte_code.cc"],
    deps = [
        ":emitter",
        "//ast:expression",
        "//module",
        "//semantic_analysis:context",
        "//semantic_analysis:instruction_set",
        "//semantic_analysis:type_system",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:core",
    ],
)

cc_library(
    name = "emitter",
    hdrs = ["emitter.h"],
    srcs = [
        "access.cc",
        "assignment.cc",
        "binary_operator.cc",
        "builtin.cc",
        "call.cc",
        "cast.cc",
        "comparison_operator.cc",
        "declaration.cc",
        "default_initialize.cc",
        "identifier.cc",
        "if_stmt.cc",
        "import.cc",
        "function_type.cc",
        "function_literal.cc",
        "module.cc",
        "return_stmt.cc",
        "slice_type.cc",
        "terminal.cc",
        "unary_operator.cc",
    ],
    deps = [
        "//ast",
        "//module",
        "//semantic_analysis:context",
        "//semantic_analysis:instruction_set",
        "//semantic_analysis:type_system",
        "//semantic_analysis/type_verification:casting",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:op_code",
        "@asoffer_jasmin//jasmin/instructions:core",
        "@asoffer_nth//nth/container:flyweight_map",
    ],
)

cc_test(
      name = "access_test",
      srcs = ["access_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "assignment_test",
      srcs = ["assignment_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "binary_operator_test",
      srcs = ["binary_operator_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "builtin_test",
      srcs = ["builtin_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "call_test",
      srcs = ["call_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "comparison_operator_test",
      srcs = ["comparison_operator_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "cast_test",
      srcs = ["cast_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "function_literal_test",
      srcs = ["function_literal_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "function_type_test",
      srcs = ["function_type_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "identifier_test",
      srcs = ["identifier_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "if_stmt_test",
      srcs = ["if_stmt_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "import_test",
      srcs = ["import_test.cc"],
      deps = [
          ":byte_code",
          "//module:specified_module_map",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "slice_type_test",
      srcs = ["slice_type_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "terminal_test",
      srcs = ["terminal_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)

cc_test(
      name = "unary_operator_test",
      srcs = ["unary_operator_test.cc"],
      deps = [
          ":byte_code",
          "//semantic_analysis:type_system",
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)
