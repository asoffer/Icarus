package(default_visibility = ["//visibility:public"])
load("//:common.bzl", "LLVM_LINKOPTS")

cc_library(
    name = "cyclic_dependency_tracker",
    hdrs = ["cyclic_dependency_tracker.h"],
    deps = [
        ":context",
        "//ast",
        "//diagnostic/consumer",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "cyclic_dependency_tracker_test",
    srcs = ["cyclic_dependency_tracker_test.cc"],
    deps = [
        ":context",
        ":cyclic_dependency_tracker",
        ":module",
        "//ast",
        "//base:ptr_span",
        "//diagnostic/consumer:tracking",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "emit_function_call_infrastructure",
    hdrs = ["emit_function_call_infrastructure.h"],
    srcs = ["emit_function_call_infrastructure.cc"],
    deps = [
        ":compiler_header",
        ":extract_jumps",
        "//base:ptr_span",
        "//ast/scope:exec",
        "//ast/scope:fn"
    ],
)

cc_library(
    name = "transient_state",
    hdrs = ["transient_state.h"],
    deps = [
        "//ast",
        "//ir/blocks:basic",
        "//ir/instruction:core",
        "//ir/value:label",
        "@com_google_absl//absl/types:span",
    ],
)

TYPE_VERIFICATION = [
    "//compiler/verify:access",
    "//compiler/verify:array_literal",
    "//compiler/verify:array_type",
    "//compiler/verify:assignment",
    "//compiler/verify:binary_operator",
    "//compiler/verify:block_literal",
    "//compiler/verify:call",
    "//compiler/verify:cast",
    "//compiler/verify:common",
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
    "//compiler/verify:jump",
    "//compiler/verify:miscellaneous",
    "//compiler/verify:parameterized_struct_literal",
    "//compiler/verify:short_function_literal",
    "//compiler/verify:struct_literal",
    "//compiler/verify:scope_literal",
    "//compiler/verify:terminal",
    "//compiler/verify:unary_operator",
]

# TODO split this up and rename the parts.
cc_library(
    name = "visitors",
    hdrs = [],
    srcs = ["emit_value.cc"],
    deps = [
        ":compiler_header",
        ":context",
        ":emit_function_call_infrastructure",
        ":executable_module",
        ":library_module",
        ":module",
        "//ast:ast",
        "//ast:overload_set",
        "//ast:visitor",
        "//base:debug",
        "//base:defer",
        "//base:guarded",
        "//base:move_func",
        "//base:permutation",
        "//base:tag",
        "//compiler/emit:access",
        "//compiler/emit:array_literal",
        "//compiler/emit:array_type",
        "//compiler/emit:binary_operator",
        "//compiler/emit:block_literal",
        "//compiler/emit:call",
        "//compiler/emit:cast",
        "//compiler/emit:comparison_operator",
        "//compiler/emit:common",
        "//compiler/emit:goto",
        "//compiler/emit:identifier",
        "//compiler/emit:index",
        "//compiler/emit:miscellaneous",
        "//compiler/emit:scope_node",
        "//compiler/emit:special",
        "//compiler/emit:terminal",
        "//compiler/emit:unary_operator",
        "//core:call",
        "//diagnostic/consumer",
        "//diagnostic/consumer:trivial",
        "//frontend:parse",
        "//frontend/source:file_name",
        "//ir/interpretter:evaluate",
        "//ir/value:addr",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//ir/value:builtin_fn",
        "//ir/value:fn",
        "//ir/value:reg",
        "//ir/value:string",
        "//module:module",
        "//type:cast",
        "//type:function",
        "//type:generic_function",
        "//type:generic_struct",
        "//type:jump",
        "//type:overload_set",
        "//type:pointer",
        "//type:qual_type",
        "//type:struct",
        "//type:type",
        "//type:type_fwd",
        "//type:typed_value",
        "//type:visitor",
        "@com_google_absl//absl/algorithm:container",
        "@com_google_absl//absl/container:node_hash_map",
        "@com_google_absl//absl/random",
    ],
)

cc_library(
    name = "compiler_header",
    hdrs = ["compiler.h"],
    deps = [
        ":context",
        ":cyclic_dependency_tracker",
        ":module",
        ":transient_state",
        "//ast",
        "//ast:overload_set",
        "//ast:visitor",
        "//base:debug",
        "//base:move_func",
        "//diagnostic/consumer",
        "//frontend/source",
        "//ir:builder",
        "//ir/instruction:set",
        "//ir/value:addr",
        "//ir/value:module_id",
        "//ir/value:reg",
        "//module",
        "//module:importer",
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
        ":verify_type",
        ":visitors",
        "//base:defer",
        "//base:log",
        "//diagnostic/consumer",
    ] + TYPE_VERIFICATION,
    alwayslink = True, # TODO: Figure out a good way to remove this.
)

cc_library(
    name = "context",
    hdrs = ["context.h"],
    srcs = ["context.cc"],
    deps = [
        "//ast:ast",
        "//base:guarded",
        "//ir:builder",
        "//ir:compiled_block",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//ir:compiled_scope",
        "//module:module",
        "//type:qual_type",
    ],
)

cc_library(
    name = "extract_jumps",
    hdrs = ["extract_jumps.h"],
    srcs = ["extract_jumps.cc"],
    deps = [
        "//ast:visitor",
        "//ast:ast",
        "//base:scope",
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
        "//ir/interpretter:evaluate",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
        "//module:module",
        "//type:type_fwd",
    ],
)

cc_library(
    name = "library_module",
    hdrs = ["library_module.h"],
    deps = [
        ":compiler_header",
        ":extract_jumps",
        ":module",
        "//diagnostic/consumer",
        "//module:importer",
    ],
)

cc_library(
    name = "executable_module",
    hdrs = ["executable_module.h"],
    deps = [
        ":compiler_header",
        ":emit_function_call_infrastructure",
        ":extract_jumps",
        ":library_module",
        ":module",
        "//diagnostic/consumer",
        "//ir:compiled_fn",
        "//module:importer",
    ],
)

cc_binary(
    name = "dump_cfg",
    srcs = ["dump_cfg.cc"],
    deps = [
        ":compiler",
        ":executable_module",
        "//base:defer",
        "//base:log",
        "//base:no_destructor",
        "//base:untyped_buffer",
        "//diagnostic/consumer:streaming",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//init:cli",
        "//ir/interpretter:evaluate",
        "//ir:compiled_fn",
        "//module",
        "//opt",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_binary(
    name = "compile",
    srcs = ["main.cc"],
    linkopts = LLVM_LINKOPTS,
    deps = [
        ":compiler",
        ":executable_module",
        "//base:defer",
        "//base:log",
        "//backend:function",
        "//backend:type",
        "//base:no_destructor",
        "//base:untyped_buffer",
        "//diagnostic/consumer:streaming",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//init:cli",
        "//ir/interpretter:evaluate",
        "//ir:compiled_fn",
        "//module",
        "//opt",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/time",
    ],
)

cc_binary(
    name = "interpret",
    srcs = ["interpretter.cc"],
    deps = [
        ":compiler",
        ":executable_module",
        "//base:defer",
        "//base:log",
        "//base:no_destructor",
        "//base:untyped_buffer",
        "//diagnostic/consumer:streaming",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//init:cli",
        "//ir/interpretter:evaluate",
        "//ir:compiled_fn",
        "//module",
        "//opt",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/time",
    ],
)

cc_library(
    name = "verify_type",
    deps = [
    ],
)
