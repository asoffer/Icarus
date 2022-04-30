package(default_visibility = ["//visibility:public"])


cc_library(
    name = "bound_parameters",
    hdrs = ["bound_parameters.h"],
    srcs = ["bound_parameters.cc"],
    deps = [
        "//ast:declaration",
        "//ir/value:result_buffer",
        "//type:qual_type",
        "@com_google_absl//absl/container:btree",
    ],
)

cc_test(
    name = "builtin_module_test",
    srcs = ["builtin_module_test.cc"],
    deps = [
        ":instructions",
        "//ir/interpreter",
        "//ir/value:scope_context",
        "//ir/value:matcher",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "call_metadata",
    hdrs = ["call_metadata.h"],
    srcs = ["call_metadata.cc"],
    deps = [
        "//ast:expression",
        "//module",
        "//type:block",
        "//type:function",
        "//type:generic",
        "//type:struct",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "common",
    hdrs = ["common.h"],
    srcs = ["common.cc"],
    deps = [
        ":module",
        "//ast:node",
        "//ast:scope",
        "//base:debug",
        "//module",
    ],
)

cc_library(
    name = "common_diagnostics",
    hdrs = ["common_diagnostics.h"],
    srcs = ["common_diagnostics.cc"],
    deps = [
        ":common",
        ":context",
        ":type_for_diagnostic",
        "//ast",
        "//core:call",
        "//diagnostic:message",
        "//type",
        "//type:callable",
    ]
)

cc_library(
    name = "flags",
    hdrs = ["flags.h"],
    srcs = ["flags.cc"],
    deps = [
        "//diagnostic/consumer",
        "//diagnostic/consumer:json",
        "//diagnostic/consumer:streaming",
        "//frontend:source_indexer",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/status:statusor",
    ],
)

cc_library(
    name = "importer",
    hdrs = ["importer.h"],
    srcs = ["importer.cc"],
    deps = [
        ":context",
        ":module",
        ":instructions",
        ":resources",
        ":work_graph",
        "//base:file",
        "//module",
        "//module:map",
        "//diagnostic/consumer",
        "//frontend:source_indexer",
        "//frontend:parse",
        "//ir/value:module_id",
        "//precompiled:module",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "instructions",
    hdrs = [
        "instructions.h",
        "builtin_module.h",
    ],
    srcs = [
        "instructions.cc",
        "builtin_module.cc",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//core:call",
        "//module:builtin",
        "//ir:subroutine",
        "//ir/byte_code",
        "//ir/instruction",
        "//ir/instruction:arithmetic",
        "//ir/instruction:compare",
        "//ir/instruction:core",
        "//ir/instruction:debug",
        "//ir/interpreter:execution_context",
        "//ir/instruction:set",
        "//ir/interpreter:evaluate",
        "//ir/value:char",
        "//ir/value:generic_fn",
        "//ir/value:module_id",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//ir/value:result_buffer",
        "//ir/value:slice",
        "//type:array",
        "//type:block",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:function_instructions",
        "//type:opaque",
        "//type:pointer",
        "//type:pointer_instructions",
        "//type:scope",
        "//type:slice",
        "//type:struct",
        "//type:generic",
        "//type",
    ],
)

cc_library(
    name = "subroutine_scaffolding",
    hdrs = ["subroutine_scaffolding.h"],
    deps = [
        "//ast:scope",
        "//ir:basic_block",
        "//ir:subroutine",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "resources",
    hdrs = ["resources.h"],
    deps = [
        ":context",
        ":transient_state",
        ":work_item",
        "//ast:node",
        "//diagnostic/consumer:buffering",
        "//ir/value:result_buffer",
        "//ir/value:scope",
        "//module:shared_context",
        "//type",
    ],
)

cc_library(
    name = "transient_state",
    hdrs = ["transient_state.h"],
    deps = [
        ":subroutine_scaffolding",
        "//core:cycle_tracker",
    ],
)

IR_EMISSION = [
    "//compiler/emit:access",
    "//compiler/emit:array_literal",
    "//compiler/emit:array_type",
    "//compiler/emit:assignment",
    "//compiler/emit:binary_operator",
    "//compiler/emit:block_node",
    "//compiler/emit:call",
    "//compiler/emit:cast",
    "//compiler/emit:comparison_operator",
    "//compiler/emit:declaration",
    "//compiler/emit:designated_initializer",
    "//compiler/emit:enum_literal",
    "//compiler/emit:function_literal",
    "//compiler/emit:function_type",
    "//compiler/emit:identifier",
    "//compiler/emit:index",
    "//compiler/emit:miscellaneous",
    "//compiler/emit:parameterized_struct_literal",
    "//compiler/emit:pattern_match",
    "//compiler/emit:return_stmt",
    "//compiler/emit:scope_literal",
    "//compiler/emit:scope_node",
    "//compiler/emit:slice_type",
    "//compiler/emit:short_function_literal",
    "//compiler/emit:struct_literal",
    "//compiler/emit:terminal",
    "//compiler/emit:unary_operator",
    "//compiler/emit:yield_stmt",
]

cc_library(
    name = "compilation_data",
    hdrs = ["compilation_data.h"],
    deps = [
        ":context",
        ":resources",
        ":transient_state",
        "//ast",
        "//base:any_invocable",
        "//diagnostic/consumer",
        "//ir/value:result_buffer",
        "//module:importer",
        "//module",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "compiler_header",
    hdrs = ["compiler.h"],
    deps = [
        ":bound_parameters",
        ":common",
        ":compilation_data",
        ":context",
        ":instructions",
        ":resources",
        ":transient_state",
        ":work_item",
        "//ast",
        "//base:debug",
        "//core:call",
        "//diagnostic/consumer",
        "//diagnostic/consumer:buffering",
        "//ir/instruction:set",
        "//ir/value:addr",
        "//ir/value:module_id",
        "//ir/value:reg",
        "//module",
        "//module:importer",
        "//type:block",
        "//type:cast",
        "//type:slice",
        "//type:type_fwd",
        "//type:qual_type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "compiler",
    deps = [
        ":compiler_header",
        ":context",
        ":instructions",
        "//ast",
        "//base:log",
        "//compiler/emit:compiler_common",
        "//compiler/verify",
        "//ir:subroutine",
        "//ir/interpreter:evaluate",
        "//type:generic",
    ] + IR_EMISSION,
)

cc_library(
    name = "context",
    hdrs = ["context.h"],
    srcs = ["context.cc"],
    deps = [
        ":bound_parameters",
        ":call_metadata",
        "//ast:ast",
        "//base:guarded",
        "//ir/byte_code",
        "//ir:subroutine",
        "//ir:module",
        "//module",
        "//type:qual_type",
        "//type:slice",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "instantiate",
    hdrs = ["instantiate.h"],
    srcs = ["instantiate.cc"],
    deps = [
        ":common",
        ":common_diagnostics",
        ":compiler_header",
        ":context",
        ":module",
        "//compiler/emit:common",
        "//core:arguments",
        "//ir/value:result_buffer",
        "//type:typed_value",
        "//type:cast",
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    deps = [
        ":context",
        ":instructions",
        "//module",
        "//precompiled:module",
        "//precompiled:module_cc_proto",
        "//precompiled:serialize",
    ],
)

cc_library(
    name = "type_for_diagnostic",
    hdrs = ["type_for_diagnostic.h"],
    srcs = ["type_for_diagnostic.cc"],
    deps = [
        ":context",
        "//ast",
        "//type:enum",
        "//type:flags",
        "//type:slice",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "type_for_diagnostic_test",
    srcs = ["type_for_diagnostic_test.cc"],
    deps = [
        ":module",
        ":type_for_diagnostic",
        ":context",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)


# cc_binary(
#     name = "dump_cfg",
#     srcs = ["dump_cfg.cc"],
#     deps = [
#         ":flags",
#         ":module",
#         ":importer",
#         ":work_graph",
#         "//base:log",
#         "//base:untyped_buffer",
#         "//ir/interpreter:evaluate",
#         "//ir:subroutine",
#         "//module",
#         "//module:shared_context",
#         "//opt",
#         "@com_google_absl//absl/debugging:failure_signal_handler",
#         "@com_google_absl//absl/debugging:symbolize",
#         "@com_google_absl//absl/flags:flag",
#         "@com_google_absl//absl/flags:parse",
#         "@com_google_absl//absl/strings",
#         "@com_google_absl//absl/strings:str_format",
#     ],
# )

cc_binary(
    name = "compile",
    srcs = ["main.cc"],
    deps = [
        ":flags",
        ":module",
        ":importer",
        ":work_graph",
        "//base:log",
        "//base:no_destructor",
        "//base:untyped_buffer",
        "//ir/interpreter:evaluate",
        "//ir:subroutine",
        "//module",
        "//module:map",
        "//module:map_bazel",
        "//module:shared_context",
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
        ":flags",
        ":module",
        ":importer",
        ":instructions",
        ":work_graph",
        "//base:log",
        "//base:untyped_buffer",
        "//ir/interpreter:evaluate",
        "//ir:subroutine",
        "//module",
        "//module:map",
        "//module:map_bazel",
        "//module:shared_context",
        "//opt",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/functional:bind_front",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/time",
        "@com_google_absl//absl/types:span",
        "@github_nlohmann_json//:json",
    ],
)

cc_library(
    name = "work_graph",
    hdrs = ["work_graph.h"],
    srcs = ["work_graph.cc"],
    deps = [
        ":compiler",
        ":instructions",
        ":resources",
        ":work_item",
        "//ir/interpreter",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "work_item",
    hdrs = ["work_item.h"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
        "//base:meta",
    ],
)

cc_library(
    name = "work_resources",
    hdrs = ["work_resources.h"],
    deps = [
        ":work_item",
        "//ast:expression",
        "//diagnostic/consumer",
        "//ir/value:result_buffer",
        "//module:shared_context",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_library(
    name = "struct",
    hdrs = ["struct.h"],
    srcs = ["struct.cc"],
    deps = [
        ":compilation_data",
        ":instructions",
        "//ast:declaration",
        "//type:struct",
        "@com_google_absl//absl/types:span",
    ],
)
