package(default_visibility = ["//visibility:public"])

cc_library(
    name = "byte_code",
    hdrs = ["byte_code.h"],
    srcs = ["byte_code.cc"],
    deps = [
        ":emitter",
        "//ast:expression",
        "//semantic_analysis:compiler_state",
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
        "builtin.cc",
        "call.cc",
        "declaration.cc",
        "default_initialize.cc",
        "identifier.cc",
        "function_type.cc",
        "function_literal.cc",
        "return_stmt.cc",
        "terminal.cc",
        "unary_operator.cc",
    ],
    deps = [
        "//ast",
        "//ir/value:slice",
        "//semantic_analysis:compiler_state",
        "//semantic_analysis:context",
        "//semantic_analysis:instruction_set",
        "//semantic_analysis:type_system",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:core",
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
