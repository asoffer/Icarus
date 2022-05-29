package(default_visibility = ["//visibility:public"])

cc_library(name = "type_xmacro", textual_hdrs = ["type.xmacro.h"])

cc_library(
    name = "argument",
    hdrs = ["argument.h"],
    deps = [
        ":type",
        "//base:untyped_buffer_view",
    ],
)

cc_library(
    name = "block",
    hdrs = ["block.h"],
    srcs = ["block.cc"],
    deps = [
        ":callable",
        ":qual_type",
        ":type",
        ":typed_value",
        "//base:global",
        "//core:parameters",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)

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
        ":system",
        ":type",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//base:global",
        "//base:no_destructor",
        "//core:arch",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//ir/interpreter",
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
        ":qual_type",
        "//core:parameters",
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
        "//diagnostic:message",
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
        ":primitive",
        ":system",
        ":type",
        ":typed_value",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//base:iterator",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//ir/interpreter",
        "//module",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/random:distributions",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "flags",
    hdrs = ["flags.h"],
    srcs = ["flags.cc"],
    deps = [
        ":primitive",
        ":system",
        ":type",
        ":typed_value",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//base:iterator",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//ir/interpreter",
        "//module",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/random:distributions",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "function",
    hdrs = ["function.h"],
    srcs = ["function.cc"],
    deps = [
        ":callable",
        ":qual_type",
        ":system",
        ":type",
        ":typed_value",
        "//base:global",
        "//core:parameters",
        "@com_google_absl//absl/container:node_hash_set",
    ],
)

cc_library(
    name = "function_instructions",
    hdrs = ["function_instructions.h"],
    deps = [
        ":function",
        ":qual_type",
        ":system",
        ":type",
        ":typed_value",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//core:parameters",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//ir/interpreter",
        "@com_google_absl//absl/container:node_hash_set",
    ],
)

cc_library(
    name = "generic",
    hdrs = ["generic.h"],
    deps = [
        ":type",
        ":type_fwd",
        ":typed_value",
        "//base:any_invocable",
        "//compiler:work_resources",
        "//core:arch",
        "//core:arguments",
    ],
)

cc_library(
    name = "generic_function",
    hdrs = ["generic_function.h"],
    deps = [
        ":argument",
        ":function",
        ":qual_type",
        ":slice",
        ":type",
        "//ir:subroutine",
        "//ir/interpreter",
        "//ir/value:slice",
    ],
)

cc_library(
    name = "opaque",
    hdrs = ["opaque.h"],
    srcs = ["opaque.cc"],
    deps = [
        ":system",
        ":type",
        "//base:debug",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//ir/interpreter",
        "//ir/value:module_id",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "overload_set",
    hdrs = ["overload_set.h"],
    srcs = ["overload_set.cc"],
    deps = [":type"],
)

cc_library(
    name = "pointer",
    hdrs = ["pointer.h"],
    srcs = ["pointer.cc"],
    deps = [
        ":system",
        ":type",
        "//base:global",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "pointer_instructions",
    hdrs = ["pointer_instructions.h"],
    deps = [
        ":pointer",
        ":system",
        ":type",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "//ir/interpreter",
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
        ":argument",
        ":system",
        ":type",
        "//base:meta",
        "//base:global",
        "//ir/value:char",
        "//ir/value:integer",
        "//ir/value:scope",
        "//ir/value:scope_context",
        "//ir/value:module_id",
        "@com_google_absl//absl/functional:function_ref",
    ],
)

cc_test(
    name = "primitive_test",
    srcs = ["primitive_test.cc"],
    deps = [
        ":primitive",
        "//base:universal_print",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
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
        ":generic",
        ":overload_set",
        "//ir/value:module_id",
        "//module",
        "//module:table",
    ],
)

cc_library(
    name = "scope",
    hdrs = ["scope.h"],
    srcs = ["scope.cc"],
    deps = [
        ":callable",
        ":qual_type",
        ":type",
        ":typed_value",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//base:global",
        "//core:parameters",
        "//ir/instruction:base",
        "//ir/instruction:debug",
        "@com_google_absl//absl/container:node_hash_map",
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
        "//ir/interpreter",
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
        "//ir:subroutine",
        "//ir/instruction:base",
        "//ir/interpreter",
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
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "type",
    hdrs = [
        "type.h",
    ],
    deps = [
        "//base:cast",
        "//base:visitable",
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
    name = "system",
    hdrs = ["system.h"],
    deps = [
        ":type",
        "//base:iterator",
        "//base:flyweight_set",
    ],
)
