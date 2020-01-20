package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_library(name = "type_xmacro", textual_hdrs = ["type.xmacro.h"])

cc_lib(
    name = "type_fwd",
    deps = [":type_xmacro"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "array",
    deps = [
        ":function",
        ":pointer",
        ":type",
        "//base:guarded",
        "//base:lazy",
        "//base:tuple",
        "//core:fn_params",
        "//core:arch",
        "//module:module",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    test_deps = None,
)

cc_lib(
    name = "callable",
    deps = [":type"],
    header_only = True,
    test_deps = None,
)

CONCRETE_TYPES = [
    ":array",
    ":enum",
    ":flags",
    ":function",
    ":jump",
    ":opaque",
    ":parameter_pack",
    ":pointer",
    ":primitive",
    ":struct",
    ":tuple",
    ":type",
    ":variant",
]

cc_library(
    name = "cast",
    hdrs = ["cast.h"],
    srcs = ["cast.cc"],
    deps = CONCRETE_TYPES + [
        "@com_google_absl//absl/algorithm:container",
    ],
)

cc_test(
    name = "cast_test",
    srcs = ["cast_test.cc"],
    deps = [
        ":array",
        ":cast",
        ":function",
        ":pointer",
        ":primitive",
        ":tuple",
        ":variant",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_lib(
    name = "enum",
    deps = [
        ":type",
        ":typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "flags",
    deps = [
        ":type",
        ":typed_value",
        "//ir:values",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/random:distributions",
    ],
    test_deps = None,
)

cc_lib(
    name = "function",
    deps = [
        ":callable",
        ":typed_value",
        "//ast:ast",
        "//core:fn_params",
        "//base:guarded",
    ],
    test_deps = None,
)

cc_lib(
    name = "generic_struct",
    deps = [
        ":callable",
        ":type",
        "//module:module",
        "//ast/scope:scope",
        "//ast/scope:module",
    ],
    test_deps = None,
)

cc_lib(
    name = "jump",
    deps = [
        ":type",
        ":function",
        "//base:guarded",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:span",
    ],
    test_deps = None,
)

cc_lib(
    name = "opaque",
    deps = [
        ":type",
        "//base:debug",
    ],
    test_deps = None,
)

cc_lib(
    name = "pointer",
    deps = [
        ":function",
        ":type",
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    test_deps = None,
)

cc_lib(
    name = "parameter_pack",
    deps = [
        ":type",
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "basic_type",
    textual_hdrs = ["primitive.xmacro.h"],
    deps = ["//base:debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "primitive",
    textual_hdrs = ["primitive.xmacro.h"],
    deps = [
        ":type",
        ":array",
        ":pointer",
    ],
    test_deps = None,
)

cc_library(
    name = "qual_type",
    hdrs = ["qual_type.h"],
    deps = [
        ":type",
        ":tuple",
    ],
)

cc_test(
    name = "qual_type_test",
    srcs = ["qual_type_test.cc"],
    deps = [
        ":primitive",
        ":qual_type",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_lib(
    name = "struct",
    deps = [
        ":function",
        ":pointer",
        ":type",
        "//ast/scope:scope",
        "//ast:hashtag",
        "//base:guarded",
        "//base:lazy",
        "//core:arch",
        "//ir:any_func",
        "//module:module",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "tuple",
    deps = [
        ":function",
        ":pointer",
        ":type",
        "//base:lazy",
        "//base:guarded",
        "//base:permutation",
        "//core:arch",
        "//ir:any_func",
        "//module:module",
    ],
    test_deps = None,
)

cc_lib(
    name = "typed_value",
    deps = [
        "//base:stringify",
        "@com_google_absl//absl/strings",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "type",
    textual_hdrs = ["primitive.xmacro.h"],
    deps = [
        ":basic_type",
        ":visitor_base",
        "//base:cast",
        "//base:debug",
        "//base:meta",
        "//base:tag",
        "//core:arch",
        "//ir:addr",
        "//ir:reg",
        "//ir:results",
        "//ir:values",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    test_deps = None,
)

cc_lib(
    name = "util",
    deps = CONCRETE_TYPES,
    header_only = True,
    test_deps = None,
)

cc_library(
    name = "variant",
    hdrs = ["variant.h"],
    srcs = ["variant.cc"],
    deps = [
        ":primitive",
        ":type",
        ":tuple",
        "//base:guarded",
        "@com_google_absl//absl/algorithm:container",
    ],
)

cc_test(
    name = "variant_test",
    srcs = ["variant_test.cc"],
    deps = [
        ":variant",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_lib(
    name = "visitor_base",
    deps = [
        ":type_fwd",
        "//base:debug",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "visitor",
    deps = [
        ":type",
        ":visitor_base",
    ],
    header_only = True,
    test_deps = None,
)
