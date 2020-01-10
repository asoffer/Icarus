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
        "verify_type.cc",
    ],
    deps = [
        ":compiler_header",
        ":data",
        ":emit_function_call_infrastructure",
        ":executable_module",
        ":extract_jumps",
        ":library_module",
        ":module",
        ":special_function",
        ":verify_assignment_and_initialization",
        "//ast:ast",
        "//ast:overload_set",
        "//ast:visitor",
        "//base:debug",
        "//base:guarded",
        "//base:move_func",
        "//base:permutation",
        "//base:tag",
        "//compiler/dispatch:fn_call_table",
        "//compiler/dispatch:scope_table",
        "//diagnostic:errors",
        "//diagnostic/consumer",
        "//diagnostic/consumer:streaming",
        "//error:log",
        "//frontend:parse",
        "//frontend/source:file_name",
        "//interpretter:evaluate",
        "//ir:addr",
        "//ir:any_func",
        "//ir:builder",
        "//ir:builtin_ir",
        "//ir:compiled_fn",
        "//ir:components",
        "//ir:jump",
        "//ir:results",
        "//ir:reg",
        "//ir:str",
        "//ir:struct_field",
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
        ":constant_binding",
        ":data",
        ":module",
        "//ast:ast_fwd",
        "//ast:overload_set",
        "//ast:visitor",
        "//base:debug",
        "//base:move_func",
        "//diagnostic/consumer",
        "//error:log",
        "//frontend/source",
        "//ir:addr",
        "//ir:builder",
        "//ir:reg",
        "//ir:results",
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
        ":constant_binding",
        "//ast:ast",
        "//base:guarded",
        "//base:lazy_convert",
        "//compiler/dispatch:fn_call_table_header",
        "//compiler/dispatch:scope_table_header",
        "//error:log",
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
    ],
    test_deps = None,
)

cc_lib(
    name = "constant_binding",
    deps = [
        "//ast:ast",
        "//base:untyped_buffer",
        "//core:arch",
        "//core:bytes",
        "//ir:results",
        "//type:type_fwd",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "module",
    deps = [
        ":constant_binding",
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
    test_deps = None,
)

cc_library(
    name = "library_module",
    hdrs = ["library_module.h"],
    deps = [
        ":compiler_header",
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
