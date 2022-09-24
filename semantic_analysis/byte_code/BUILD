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
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:core",
    ],
)

cc_library(
    name = "emitter",
    hdrs = ["emitter.h"],
    srcs = [
        "terminal.cc",
    ],
    deps = [
        ":instruction_set",
        "//ast",
        "//semantic_analysis:context",
        "//semantic_analysis:type_system",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:core",
    ],
)

cc_library(
    name = "instruction_set",
    hdrs = ["instruction_set.h"],
    deps = [
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:core",
    ],
)

cc_test(
      name = "terminal_test",
      srcs = ["terminal_test.cc"],
      deps = [
          "//test:repl",
          "@com_google_googletest//:gtest_main",
      ],
)
