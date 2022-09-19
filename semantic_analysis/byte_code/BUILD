package(default_visibility = ["//visibility:public"])

cc_library(
    name = "byte_code",
    hdrs = ["byte_code.h"],
    srcs = ["byte_code.cc"],
    deps = [
        ":emitter",
        ":instruction_set",
        "//ast:expression",
        "//compiler:context",
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
        "//compiler:context",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:core",
    ],
)

cc_library(
    name = "instruction_set",
    hdrs = ["instruction_set.h"],
    deps = [
        "//core/type_system",
        "//core/type_system:builtin",
        "//core/type_system:function",
        "//core/type_system:parameter",
        "//core/type_system:pointer",
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
