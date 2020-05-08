package(default_visibility = ["//visibility:public"])

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


# TODO split this up and rename the parts.
cc_library(
    name = "visitors",
    hdrs = [],
    srcs = [
        "emit_assign.cc",
        "emit_copy_init.cc",
        "emit_default_init.cc",
        "emit_destroy.cc",
        "emit_move_init.cc",
        "emit_ref.cc",
        "emit_value.cc",
        "emit_chainop.cc",
    ],
    deps = [
        ":compiler_header",
        ":data",
        ":emit_function_call_infrastructure",
        ":executable_module",
        ":library_module",
        ":module",
        ":special_function",
        ":verify_type",
        "//ast:ast",
        "//ast:overload_set",
        "//ast:visitor",
        "//base:debug",
        "//base:defer",
        "//base:guarded",
        "//base:move_func",
        "//base:permutation",
        "//base:tag",
        "//compiler/dispatch:fn_call_table",
        "//compiler/dispatch:scope_table",
        "//core:call",
        "//diagnostic:errors",
        "//diagnostic/consumer",
        "//diagnostic/consumer:trivial",
        "//frontend:parse",
        "//frontend/source:file_name",
        "//interpretter:evaluate",
        "//ir/value:addr",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:jump",
        "//ir:results",
        "//ir:struct_field",
        "//ir/value:builtin_fn",
        "//ir/value:fn",
        "//ir/value:reg",
        "//ir/value:string",
        "//module:module",
        "//module:pending",
        "//type:cast",
        "//type:function",
        "//type:generic_function",
        "//type:generic_struct",
        "//type:jump",
        "//type:overload_set",
        "//type:parameter_pack",
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
    alwayslink = 1,
)

cc_library(
    name = "compiler_header",
    hdrs = ["compiler.h"],
    deps = [
        ":data",
        ":module",
        "//ast:ast_fwd",
        "//ast:overload_set",
        "//ast:visitor",
        "//base:debug",
        "//base:move_func",
        "//compiler/constant:binding",
        "//diagnostic/consumer",
        "//frontend/source",
        "//ir/value:addr",
        "//ir:builder",
        "//ir:results",
        "//ir/value:reg",
        "//module",
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
        "//diagnostic/consumer",
    ],
)

cc_library(
    name = "special_function",
    hdrs = ["special_function.h"],
    srcs = ["special_function.cc"],
    deps = [
        ":compiler_header",
        "//ir/value:fn",
        "//ast",
        "//type:function",
        "//type:pointer",
        "//type:struct",
    ],
)

# TODO rename
cc_library(
    name = "data",
    hdrs = ["data.h"],
    srcs = ["data.cc"],
    deps = [
        "//ast:ast",
        "//base:guarded",
        "//base:lazy_convert",
        "//compiler/constant:binding",
        "//ir:block_def",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:jump",
        "//ir:results",
        "//ir:scope_def",
        "//module:module",
        "//module:pending",
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
        ":data",
        "//ast:ast",
        "//ast:ast_fwd",
        "//base:ptr_span",
        "//interpretter:evaluate",
        "//ir:compiled_fn",
        "//ir:jump",
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
    ],
)

cc_library(
    name = "executable_module",
    hdrs = ["executable_module.h"],
    deps = [
        ":compiler_header",
        ":emit_function_call_infrastructure",
        ":extract_jumps",
        ":module",
        "//diagnostic/consumer",
        "//ir:compiled_fn",
    ],
)


cc_library(
    name = "verify_assignment_and_initialization",
    hdrs = ["verify_assignment_and_initialization.h"],
    srcs = ["verify_assignment_and_initialization.cc"],
    deps = [
        "//diagnostic/consumer",
        "//diagnostic:errors",
        "//frontend/source:range",
        "//type:cast",
        "//type:qual_type",
    ],
)

cc_test(
    name = "verify_assignment_and_initialization_test",
    srcs = ["verify_assignment_and_initialization_test.cc"],
    deps = [
        ":verify_assignment_and_initialization",
        "//diagnostic/consumer:trivial",
        "//type:pointer",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_binary(
    name = "compile",
    srcs = ["main.cc"],
    deps = [
        ":compiler",
        ":executable_module",
        "//base:defer",
        "//base:log",
        "//base:no_destructor",
        "//base:untyped_buffer",
        "//frontend/source:file_name",
        "//frontend/source:shared",
        "//init:cli",
        "//interpretter:execute",
        "//ir:compiled_fn",
        "//module",
        "//module:pending",
        "//opt",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/strings",
    ],
)

cc_binary(
    name = "repl",
    srcs = ["repl.cc"],
    deps = [
        "//ast:ast",
        "//ast:expression",
        "//base:untyped_buffer",
        "//compiler:compiler",
        "//core:params",
        "//diagnostic/consumer:streaming",
        "//frontend/source:repl",
        "//frontend/source:shared",
        "//frontend:parse",
        "//init:cli",
        "//interpretter:execute",
        "//ir:compiled_fn",
        "//module",
        "//opt",
        "//type:function",
        "//type:tuple",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "verify_type",
    hdrs = [],
    srcs = [
        "verify_type.cc",
    ],
    deps = [
        ":compiler_header",
        ":data",
        ":library_module",
        ":module",
        ":special_function",
        ":verify_assignment_and_initialization",
        "//ast",
        "//base:debug",
        "//base:defer",
        "//compiler/dispatch:parameters_and_arguments",
        "//diagnostic/consumer",
        "//diagnostic/consumer:trivial",
        "//diagnostic:errors",
        "//frontend/source:file_name",
        "//interpretter:evaluate",
        "//ir/value:addr",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:jump",
        "//ir:results",
        "//ir:struct_field",
        "//ir/value:builtin_fn",
        "//ir/value:fn",
        "//ir/value:generic_fn",
        "//ir/value:reg",
        "//ir/value:string",
        "//module:module",
        "//module:pending",
        "//type:cast",
        "//type:function",
        "//type:generic_function",
        "//type:generic_struct",
        "//type:jump",
        "//type:overload_set",
        "//type:parameter_pack",
        "//type:pointer",
        "//type:qual_type",
        "//type:struct",
        "//type:type",
        "//type:type_fwd",
        "//type:typed_value",
        "//type:visitor",
        "@com_google_absl//absl/algorithm:container",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)
