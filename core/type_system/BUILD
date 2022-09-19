package(default_visibility = ["//visibility:public"])

cc_library(
    name = "builtin",
    hdrs = ["builtin.h"],
    deps = [
        "//base:meta",
        "//base:flyweight_set",
    ]
)

cc_test(
    name = "builtin_test",
    srcs = ["builtin_test.cc"],
    deps = [
        ":builtin",
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "parameter",
    hdrs = ["parameter.h"],
    deps = [
        "//core:parameters",
        "//base:meta",
        "//base:flyweight_set",
    ]
)

cc_test(
    name = "parameter_test",
    srcs = ["parameter_test.cc"],
    deps = [
        ":builtin",
        ":parameter",
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "pointer",
    hdrs = ["pointer.h"],
    deps = [
        "//base:meta",
        "//base:flyweight_set",
    ]
)

cc_test(
    name = "pointer_test",
    srcs = ["pointer_test.cc"],
    deps = [
        ":pointer",
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "type_system",
    hdrs = ["type_system.h"],
    deps = [
        "//base:meta",
        "//base:flyweight_set",
        "@asoffer_jasmin//jasmin:instruction",
    ]
)

cc_test(
    name = "type_system_test",
    srcs = ["type_system_test.cc"],
    deps = [
        ":builtin",
        ":pointer",
        ":type_system",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_jasmin//jasmin/instructions:core",
        "@asoffer_jasmin//jasmin:function",
        "@com_google_googletest//:gtest_main",
    ],
)
