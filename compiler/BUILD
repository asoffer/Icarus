package(default_visibility = ["//visibility:public"])
load("//:common.bzl", "LLVM_LINKOPTS")


cc_library(
    name = "bound_parameters",
    hdrs = ["bound_parameters.h"],
    srcs = ["bound_parameters.cc"],
    deps = [
        "//core:params",
        "//ir/value:result_buffer",
        "//type:qual_type",
    ],
)
cc_library(
    name = "cyclic_dependency_tracker",
    hdrs = ["cyclic_dependency_tracker.h"],
    deps = [
        "//ast",
        "//diagnostic/consumer",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_test(
    name = "cyclic_dependency_tracker_test",
    srcs = ["cyclic_dependency_tracker_test.cc"],
    deps = [
        ":cyclic_dependency_tracker",
        "//ast",
        "//diagnostic/consumer:tracking",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "instructions",
    hdrs = ["instructions.h"],
    srcs = ["instructions.cc"],
    visibility = ["//visibility:public"],
    deps = [
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type/interface:ir",
        "//type:opaque",
        "//type:pointer",
        "//type:slice",
        "//type:struct",
        "//type:generic_struct",
        "//type:generic_function",
        "//type",
        "//ir/byte_code",
        "//ir/instruction:arithmetic",
        "//ir/instruction:compare",
        "//ir/instruction:core",
        "//ir/instruction:set",
        "//ir/interpreter:evaluate",
        "//ir/value:char",
        "//ir/value:generic_fn",
        "//ir/value:module_id",
        "//ir/value:result_buffer",
    ],
)

cc_library(
    name = "resources",
    hdrs = ["resources.h"],
    srcs = ["resources.cc"],
    deps = [
        ":context",
        ":transient_state",
        "//ast:node",
        "//diagnostic/consumer",
        "//module:importer",
    ],
)

cc_library(
    name = "transient_state",
    hdrs = ["transient_state.h"],
    deps = ["//ir:scope_state"],
)

TYPE_VERIFICATION = [
    "//compiler/verify:access",
    "//compiler/verify:array_literal",
    "//compiler/verify:array_type",
    "//compiler/verify:assignment",
    "//compiler/verify:binary_operator",
    "//compiler/verify:block_literal",
    "//compiler/verify:block_node",
    "//compiler/verify:call",
    "//compiler/verify:cast",
    "//compiler/verify:comparison_operator",
    "//compiler/verify:declaration",
    "//compiler/verify:designated_initializer",
    "//compiler/verify:enum_literal",
    "//compiler/verify:function_literal",
    "//compiler/verify:function_type",
    "//compiler/verify:goto",
    "//compiler/verify:identifier",
    "//compiler/verify:import",
    "//compiler/verify:index",
    "//compiler/verify:interface_literal",
    "//compiler/verify:jump",
    "//compiler/verify:miscellaneous",
    "//compiler/verify:parameterized_struct_literal",
    "//compiler/verify:pattern_match",
    "//compiler/verify:short_function_literal",
    "//compiler/verify:slice_type",
    "//compiler/verify:struct_literal",
    "//compiler/verify:scope_literal",
    "//compiler/verify:terminal",
    "//compiler/verify:unary_operator",
]

IR_EMISSION = [
    "//compiler/emit:access",
    "//compiler/emit:array_literal",
    "//compiler/emit:array_type",
    "//compiler/emit:assignment",
    "//compiler/emit:binary_operator",
    "//compiler/emit:block_literal",
    "//compiler/emit:block_node",
    "//compiler/emit:call",
    "//compiler/emit:cast",
    "//compiler/emit:comparison_operator",
    "//compiler/emit:declaration",
    "//compiler/emit:designated_initializer",
    "//compiler/emit:enum_literal",
    "//compiler/emit:function_literal",
    "//compiler/emit:function_type",
    "//compiler/emit:goto",
    "//compiler/emit:identifier",
    "//compiler/emit:index",
    "//compiler/emit:interface_literal",
    "//compiler/emit:jump",
    "//compiler/emit:miscellaneous",
    "//compiler/emit:parameterized_struct_literal",
    "//compiler/emit:pattern_match",
    "//compiler/emit:return_stmt",
    "//compiler/emit:scope_literal",
    "//compiler/emit:scope_node",
    "//compiler/emit:slice_type",
    "//compiler/emit:short_function_literal",
    "//compiler/emit:special",
    "//compiler/emit:struct_literal",
    "//compiler/emit:terminal",
    "//compiler/emit:unary_operator",
    "//compiler/emit:yield_stmt",
]

cc_library(
    name = "compiler_header",
    hdrs = ["compiler.h"],
    deps = [
        ":bound_parameters",
        ":context",
        ":cyclic_dependency_tracker",
        ":instructions",
        ":module",
        ":resources",
        ":transient_state",
        "//ast",
        "//ast:overload_set",
        "//ast:visitor",
        "//base:debug",
        "//diagnostic/consumer",
        "//diagnostic/consumer:buffering",
        "//frontend/source",
        "//ir:builder",
        "//ir/instruction:set",
        "//ir/value:addr",
        "//ir/value:module_id",
        "//ir/value:reg",
        "//module",
        "//module:importer",
        "//type:slice",
        "//type:type_fwd",
        "//type:qual_type",
        "//type:visitor",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "compiler",
    srcs = ["compiler.cc"],
    deps = [
        ":compiler_header",
        ":context",
        ":instructions",
        ":module",
        "//ast",
        "//base:log",
        "//compiler/emit:common",
        "//compiler/verify:common",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//ir/interpreter:evaluate",
        "//type:generic_struct",
        "//type:jump",
    ] + TYPE_VERIFICATION + IR_EMISSION,
)

cc_library(
    name = "context",
    hdrs = ["context.h"],
    srcs = ["context.cc"],
    deps = [
        ":bound_parameters",
        ":jump_map",
        "//ast:ast",
        "//base:guarded",
        "//ir:builder",
        "//ir/byte_code",
        "//ir:compiled_block",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//ir:compiled_scope",
        "//ir:module",
        "//module:module",
        "//type:qual_type",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "instantiate",
    hdrs = ["instantiate.h"],
    srcs = ["instantiate.cc"],
    deps = [
        ":context",
        ":compiler_header",
        "//core:arguments",
        "//ir/value:result_buffer",
        "//type:typed_value",
    ],
)

cc_library(
    name = "jump_map",
    hdrs = ["jump_map.h"],
    srcs = ["jump_map.cc"],
    deps = [
        "//ast",
        "//ast:visitor",
        "//base:scope",
        "//base:ptr_union",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    deps = [
        ":context",
        "//ast:ast",
        "//ast:ast_fwd",
        "//base:ptr_span",
        "//ir/interpreter:evaluate",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//module",
        "@com_google_absl//absl/synchronization",
    ],
)

cc_library(
    name = "library_module",
    hdrs = ["library_module.h"],
    deps = [
        ":compiler",
        ":module",
        ":resources",
        "//diagnostic/consumer",
        "//module:importer",
    ],
)

cc_library(
    name = "executable_module",
    hdrs = ["executable_module.h"],
    deps = [
        ":compiler",
        ":library_module",
        ":module",
        "//ast:scope",
        "//diagnostic/consumer",
        "//ir:compiled_fn",
        "//module:importer",
    ],
)

cc_library(
    name = "type_for_diagnostic",
    hdrs = ["type_for_diagnostic.h"],
    srcs = ["type_for_diagnostic.cc"],
    deps = [
        ":context",
        ":module",
        "//ast",
        "//ast:visitor",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "type_for_diagnostic_test",
    srcs = ["type_for_diagnostic_test.cc"],
    deps = [
        ":library_module",
        ":type_for_diagnostic",
        ":context",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)


cc_binary(
    name = "dump_cfg",
    srcs = ["dump_cfg.cc"],
    deps = [
        ":executable_module",
        "//base:log",
        "//base:no_destructor",
        "//base:untyped_buffer",
        "//compiler:library_module",
        "//diagnostic/consumer:streaming",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//ir/interpreter:evaluate",
        "//ir:compiled_fn",
        "//module",
        "//opt",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_binary(
    name = "compile",
    srcs = ["main.cc"],
    linkopts = LLVM_LINKOPTS,
    deps = [
        ":executable_module",
        "//base:log",
        "//backend:llvm",
        "//base:no_destructor",
        "//base:untyped_buffer",
        "//diagnostic/consumer:streaming",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//ir/interpreter:evaluate",
        "//ir:compiled_fn",
        "//module",
        "//opt",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/time",
    ],
)

cc_binary(
    name = "interpret",
    srcs = ["interpreter.cc"],
    deps = [
        ":executable_module",
        ":instructions",
        "//base:log",
        "//base:no_destructor",
        "//base:untyped_buffer",
        "//diagnostic/consumer:streaming",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//ir/interpreter:evaluate",
        "//ir:compiled_fn",
        "//module",
        "//opt",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/time",
    ],
)
