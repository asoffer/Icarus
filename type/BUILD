package(default_visibility = ["//visibility:public"])

cc_library(name = "type_xmacro", textual_hdrs = ["type.xmacro.h"])

cc_library(
    name = "type_fwd",
    hdrs = ["type_fwd.h"],
    deps = [":type_xmacro"],
)

cc_library(
    name = "array",
    hdrs = ["array.h"],
    srcs = ["array.cc"],
    deps = [
        ":function",
        ":pointer",
        ":type",
        "//base:guarded",
        "//base:lazy",
        "//base:tuple",
        "//core:params",
        "//core:arch",
        "//module:module",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "callable",
    hdrs = ["callable.h"],
    deps = [":type"],
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

cc_library(
    name = "enum",
    hdrs = ["enum.h"],
    srcs = ["enum.cc"],
    deps = [
        ":type",
        ":typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "flags",
    hdrs = ["flags.h"],
    srcs = ["flags.cc"],
    deps = [
        ":type",
        ":typed_value",
        "//ir/value:enum_and_flags",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/random:distributions",
    ],
)

cc_library(
    name = "function",
    hdrs = ["function.h"],
    srcs = ["function.cc"],
    deps = [
        ":callable",
        ":typed_value",
        "//ast:ast",
        "//core:params",
        "//base:guarded",
    ],
)

cc_library(
    name = "generic_struct",
    hdrs = ["generic_struct.h"],
    srcs = ["generic_struct.cc"],
    deps = [
        ":callable",
        ":type",
        "//module:module",
        "//ast/scope:scope",
        "//ast/scope:module",
    ],
)

cc_library(
    name = "jump",
    hdrs = ["jump.h"],
    srcs = ["jump.cc"],
    deps = [
        ":type",
        ":function",
        "//base:guarded",
        "//core:params",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "opaque",
    hdrs = ["opaque.h"],
    srcs = ["opaque.cc"],
    deps = [
        ":type",
        "//base:debug",
    ],
)

cc_library(
    name = "pointer",
    hdrs = ["pointer.h"],
    srcs = ["pointer.cc"],
    deps = [
        ":function",
        ":type",
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "parameter_pack",
    hdrs = ["parameter_pack.h"],
    srcs = ["parameter_pack.cc"],
    deps = [
        ":type",
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "basic_type",
    hdrs = ["basic_type.h"],
    textual_hdrs = ["primitive.xmacro.h"],
    deps = ["//base:debug"],
)

cc_library(
    name = "primitive",
    hdrs = ["primitive.h"],
    srcs = ["primitive.cc"],
    textual_hdrs = ["primitive.xmacro.h"],
    deps = [
        ":type",
        ":array",
        ":pointer",
    ],
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

cc_library(
    name = "struct",
    hdrs = ["struct.h"],
    srcs = ["struct.cc"],
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
)

cc_library(
    name = "tuple",
    hdrs = ["tuple.h"],
    srcs = ["tuple.cc"],
    deps = [
        ":function",
        ":pointer",
        ":type",
        "//base:lazy",
        "//base:guarded",
        "//core:arch",
        "//ir:any_func",
    ],
)

cc_library(
    name = "typed_value", 
    hdrs = ["typed_value.h"],
    deps = [
        "//base:stringify",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "type",
    hdrs = ["type.h"],
    srcs = ["type.cc"],
    textual_hdrs = ["primitive.xmacro.h"],
    deps = [
        ":basic_type",
        ":visitor_base",
        "//base:cast",
        "//base:debug",
        "//base:meta",
        "//base:tag",
        "//core:arch",
        "//ir:results",
        "//ir/value:addr",
        "//ir/value:enum_and_flags",
        "//ir/value:reg",
        "//ir/value:string",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "util",
    hdrs = ["util.h"],
    deps = CONCRETE_TYPES,
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

cc_library(
    name = "visitor_base",
    hdrs = ["visitor_base.h"],
    deps = [
        ":type_fwd",
        "//base:debug",
    ],
)

cc_library(
    name = "visitor",
    hdrs = ["visitor.h"],
    deps = [
        ":type",
        ":visitor_base",
    ],
)
