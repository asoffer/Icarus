package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_library(
    name = "emit_function_call_infrastructure",
    hdrs = ["emit_function_call_infrastructure.h"],
    srcs = ["emit_function_call_infrastructure.cc"],
    deps = [
        ":compiler_header",
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
        "emit_print.cc",
        "emit_ref.cc",
        "emit_value.cc",
        "emit_chainop.cc",
        "verify_type.cc",
    ],
    deps = [
        ":compiler_header",
        ":data",
        ":emit_function_call_infrastructure",
        ":executable_module",
        ":library_module",
        ":module",
        ":special_function",
        ":verify_assignment_and_initialization",
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
        "//diagnostic:errors",
        "//diagnostic/consumer",
        "//diagnostic/consumer:streaming",
        "//frontend:parse",
        "//frontend/source:file_name",
        "//interpretter:evaluate",
        "//ir/value:addr",
        "//ir:any_func",
        "//ir:builder",
        "//ir:builtin_ir",
        "//ir:compiled_fn",
        "//ir:components",
        "//ir:jump",
        "//ir:results",
        "//ir:str",
        "//ir:struct_field",
        "//ir/value:reg",
        "//module:module",
        "//module:pending",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:jump",
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
    test_deps = None,
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
        ":visitors",
        "//base:defer",
        "//diagnostic/consumer:streaming",
    ],
)

cc_lib(
    name = "special_function",
    deps = [
        ":compiler_header",
        "//ir:any_func",
        "//ast",
        "//type:function",
        "//type:pointer",
        "//type:struct",
    ],
    test_deps = None,
)

# TODO rename
cc_lib(
    name = "data",
    deps = [
        "//ast:ast",
        "//base:guarded",
        "//base:lazy_convert",
        "//compiler/constant:binding",
        "//compiler/constant:binding_tree",
        "//compiler/dispatch:fn_call_table_header",
        "//compiler/dispatch:scope_table_header",
        "//diagnostic/consumer:streaming",
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
    test_deps = None,
)

cc_lib(
    name = "extract_jumps",
    deps = [
        "//ast:visitor",
        "//ast:ast",
        "//base:scope",
    ],
    test_deps = None,
)

cc_lib(
    name = "module",
    deps = [
        ":data",
        "//ast:ast",
        "//ast:ast_fwd",
        "//base:ptr_span",
        "//compiler/constant:binding_map",
        "//interpretter:evaluate",
        "//ir:compiled_fn",
        "//ir:jump",
        "//module:module",
        "//type:type_fwd",
    ],
    test_deps = None,
)

cc_library(
    name = "library_module",
    hdrs = ["library_module.h"],
    deps = [
        ":compiler_header",
        ":extract_jumps",
        ":module",
        "//diagnostic/consumer:streaming",
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
        "//diagnostic/consumer:streaming",
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
