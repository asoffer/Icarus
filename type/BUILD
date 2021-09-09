package(default_visibility = ["//visibility:public"])

cc_library(name = "type_xmacro", textual_hdrs = ["type.xmacro.h"])

cc_library(
    name =  "fake",
    hdrs = ["fake.h"],
    deps = [
        ":type",
        "//base:no_destructor",
        "//core:arch",
        "@com_google_absl//absl/strings:str_format",
    ],
    testonly = True,
)

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
        ":primitive",
        ":type",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//base:global",
        "//base:no_destructor",
        "//core:arch",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//ir/value:fn",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_test(
    name = "array_test",
    srcs = ["array_test.cc"],
    deps = [
        ":array",
        ":fake",
        "@com_google_googletest//:gtest_main",
    ],
)

CONCRETE_TYPES = [
    ":array",
    ":enum",
    ":flags",
    ":function",
    ":jump",
    ":opaque",
    ":pointer",
    ":primitive",
    ":slice",
    ":struct",
    ":type",
]

cc_library(
    name = "callable",
    hdrs = ["callable.h"],
    deps = [
        ":type",
        ":typed_value",
        "//core:arguments",
        "//ir/value:result_buffer",
    ],
)

cc_library(
    name = "cast",
    hdrs = ["cast.h"],
    srcs = ["cast.cc"],
    deps = [
        ":array",
        ":enum",
        ":flags",
        ":function",
        ":opaque",
        ":pointer",
        ":primitive",
        ":slice",
        ":type",
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
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//module",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/random:distributions",
    ],
)

cc_library(
    name = "flags",
    hdrs = ["flags.h"],
    srcs = ["flags.cc"],
    deps = [
        ":type",
        ":typed_value",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//module",
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
        ":qual_type",
        ":type",
        ":typed_value",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//base:global",
        "//core:params",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)

cc_library(
    name = "generic_function",
    hdrs = ["generic_function.h"],
    srcs = ["generic_function.cc"],
    deps = [
        ":callable",
        ":function",
        ":type",
        ":typed_value",
        "//base:any_invocable",
        "//core:arch",
        "//core:arguments",
    ],
)

cc_library(
    name = "generic_struct",
    hdrs = ["generic_struct.h"],
    srcs = ["generic_struct.cc"],
    deps = [
        ":struct",
        ":type",
        "//ast:scope",
        "//base:any_invocable",
        "//core:arguments",
        "//module",
    ],
)

cc_library(
    name = "instantiated_generic_struct",
    hdrs = ["instantiated_generic_struct.h"],
    deps = [
        ":generic_struct",
        ":struct",
        ":type",
    ],
)

cc_library(
    name = "jump",
    hdrs = ["jump.h"],
    srcs = ["jump.cc"],
    deps = [
        ":type",
        ":qual_type",
        "//base:global",
        "//core:params",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "jump_test",
    srcs = ["jump_test.cc"],
    deps = [
        ":fake",
        ":jump",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "opaque",
    hdrs = ["opaque.h"],
    deps = [
        ":type",
        "//base:debug",
        "//base:extend",
        "//base/extend:traverse",
        "//ir/instruction:base",
        "//module",
    ],
)

cc_library(
    name = "overload_set",
    hdrs = ["overload_set.h"],
    srcs = ["overload_set.cc"],
    deps = [
        ":callable",
        ":type",
    ],
)

cc_library(
    name = "pointer",
    hdrs = ["pointer.h"],
    srcs = ["pointer.cc"],
    deps = [
        ":type",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//base:global",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "pointer_test",
    srcs = ["pointer_test.cc"],
    deps = [
        ":fake",
        ":pointer",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "primitive",
    hdrs = ["primitive.h"],
    srcs = ["primitive.cc"],
    textual_hdrs = ["primitive.xmacro.h"],
    deps = [
        ":type",
        "//base:meta",
        "//base:global",
        "//ir/value:char",
        "//ir/value:integer",
        "//ir/value:module_id",
        "//ir/value:string",
        "@com_google_absl//absl/functional:function_ref",
    ],
)

cc_library(
    name = "qual_type",
    hdrs = ["qual_type.h"],
    srcs = ["qual_type.cc"],
    deps = [
        ":type",
        "//base:global",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:span",
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
    name = "provenance",
    hdrs = ["provenance.h"],
    srcs = ["provenance.cc"],
    deps = CONCRETE_TYPES + [
        ":generic_function",
        ":generic_struct",
        ":overload_set",
        ":visitor",
        "//module",
    ],
)

cc_library(
    name = "slice",
    hdrs = ["slice.h"],
    srcs = ["slice.cc"],
    deps = [
        ":primitive",
        ":type",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//base:global",
        "//base:no_destructor",
        "//core:arch",
        "//ir/value:slice",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_test(
    name = "slice_test",
    srcs = ["slice_test.cc"],
    deps = [
        ":slice",
        ":fake",
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
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//core:arch",
        "//ir:compiled_fn",
        "//ir/instruction:base",
        "//ir/interpreter:execution_context",
        "//ir/value:fn",
        "//ir/value:hashtag",
        "//module:module",
        "@com_google_absl//absl/container:flat_hash_map",
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
    deps = [
        ":visitor_base",
        "//base:cast",
        "//base:meta",
        "//core:arch",
        "//ir/value:reg",
        "//ir/value:result_buffer",
    ],
)

cc_test(
    name = "type_test",
    srcs = ["type_test.cc"],
    deps = [
        ":type",
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
