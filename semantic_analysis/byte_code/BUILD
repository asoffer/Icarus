package(default_visibility = ["//visibility:public"])

cc_library(
    name = "byte_code",
    hdrs = ["byte_code.h"],
    srcs = ["byte_code.cc"],
    deps = [
        ":emitter",
        ":instruction_set",
        "//ast:expression",
        "//semantic_analysis:context",
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
        "call.cc",
        "function_type.cc",
        "unary_operator.cc",
        "terminal.cc",
    ],
    deps = [
        ":instruction_set",
        "//ast",
        "//ir/value:slice",
        "//semantic_analysis:context",
        "//semantic_analysis:type_system",
        "//type:primitive",
        "//type:slice",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:core",
    ],
)

cc_library(
    name = "instruction_set",
    hdrs = [
        "instruction_set.h",
        "foreign_function_map.h",
    ],
    srcs = [
        "instruction_set.cc",
        "foreign_function_map.cc",
    ],
    deps = [
        "//base:flyweight_map",
        "//core/type_system",
        "//ir/value:fn",
        "//semantic_analysis:type_system",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:core",
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
