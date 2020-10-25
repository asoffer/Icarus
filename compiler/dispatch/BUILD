package(default_visibility = ["//visibility:public"]) # TODO compiler:__subpackages__"])

cc_library(
    name = "parameters_and_arguments",
    hdrs = ["parameters_and_arguments.h"],
    srcs = ["parameters_and_arguments.cc"],
    deps = [
        "//ast:ast",
        "//compiler:compiler_header",
        "//ir/interpretter:evaluate",
        "//type:cast",
        "//type:type",
        "//type:function",
        "//type:generic_function",
    ],
)

cc_test(
    name = "parameters_and_arguments_test",
    srcs = ["parameters_and_arguments_test.cc"],
    deps = [
        "//compiler:compiler",
        "//compiler:visitors",
        "//test:module",
        "//type:function",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "match",
    hdrs = ["match.h"],
    srcs = ["match.cc"],
    deps = [
        "//base:expected",
        "//type:cast",
        "//core:fn_args",
        "//core:params",
        "//type:qual_type",
        "//type:type",
        "//type:typed_value",
    ],
)

cc_test(
    name = "match_test",
    srcs = ["match_test.cc"],
    deps = [
        ":match",
        ":parameters_and_arguments",
        "//compiler:compiler",
        "//test:module",
        "//type:function",
        "@com_google_googletest//:gtest_main",
    ],
)
