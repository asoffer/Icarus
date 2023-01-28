package(default_visibility = ["//visibility:public"])

cc_library(
    name = "finite_set",
    hdrs = ["finite_set.h"],
    deps = [
        ":type_system",
        "@asoffer_nth//nth/meta:concepts",
    ]
)

cc_test(
    name = "finite_set_test",
    srcs = ["finite_set_test.cc"],
    deps = [
        ":finite_set",
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)


cc_library(
    name = "function",
    hdrs = ["function.h"],
    deps = [
        ":parameter",
        ":type_system",
    ]
)

cc_test(
    name = "function_test",
    srcs = ["function_test.cc"],
    deps = [
        ":function",
        ":parameter",
        ":sized_integer",
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "parameter",
    hdrs = ["parameter.h"],
    deps = [
        ":type_system",
        "//core:parameters",
    ]
)

cc_test(
    name = "parameter_test",
    srcs = ["parameter_test.cc"],
    deps = [
        ":sized_integer",
        ":parameter",
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "pointer",
    hdrs = ["pointer.h"],
    deps = [
        ":type_system",
    ]
)

cc_test(
    name = "pointer_test",
    srcs = ["pointer_test.cc"],
    deps = [
        ":finite_set",
        ":pointer",
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "sized_integer",
    hdrs = ["sized_integer.h"],
    deps = [
        ":type_system",
        "//core:alignment",
        "//core:bytes",
    ]
)

cc_test(
    name = "sized_integer_test",
    srcs = ["sized_integer_test.cc"],
    deps = [
        ":sized_integer",
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "type",
    hdrs = ["type.h"],
    deps = [
        "//base:debug",
    ]
)

cc_library(
    name = "type_system",
    hdrs = ["type_system.h"],
    deps = [
        ":type",
        "@asoffer_jasmin//jasmin:instruction",
        "@asoffer_nth//nth/container:flyweight_set",
        "@asoffer_nth//nth/meta:concepts",
        "@asoffer_nth//nth/meta:sequence",
        "@asoffer_nth//nth/meta:type",
    ]
)

cc_test(
    name = "type_system_test",
    srcs = ["type_system_test.cc"],
    deps = [
        ":pointer",
        ":sized_integer",
        ":type_system",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_jasmin//jasmin/instructions:core",
        "@asoffer_jasmin//jasmin:function",
        "@com_google_googletest//:gtest_main",
    ],
)
