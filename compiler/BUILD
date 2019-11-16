package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")


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
        ":dependent_data",
        ":extract_jumps",
        ":module",
        ":special_function",
        ":verify_result",
        "//ast:ast",
        "//ast/methods:dump",
        "//ast:overload_set",
        "//ast:visitor",
        "//backend:eval",
        "//base:debug",
        "//base:guarded",
        "//base:move_func",
        "//base:permutation",
        "//base:tag",
        "//compiler/dispatch:table",
        "//error:log",
        "//frontend:parse",
        "//ir:addr",
        "//ir:any_func",
        "//ir:builder",
        "//ir:builtin_ir",
        "//ir:compiled_fn",
        "//ir:components",
        "//ir:jump_handler",
        "//ir:results",
        "//ir:reg",
        "//ir:str",
        "//ir/cmd:basic",
        "//ir/cmd:call",
        "//ir/cmd:cast",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:return",
        "//ir/cmd:scope",
        "//ir/cmd:store",
        "//ir/cmd:types",
        "//module:module",
        "//module:pending",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:jump",
        "//type:parameter_pack",
        "//type:pointer",
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
        ":dependent_data",
        ":verify_result",
        "//ast:ast_fwd",
        "//ast:overload_set",
        "//ast:visitor",
        "//base:debug",
        "//base:move_func",
        "//error:log",
        "//frontend/source",
        "//ir:addr",
        "//ir:builder",
        "//ir:reg",
        "//ir:results",
        "//module",
        "//type:type_fwd",
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
        ":dependent_data",
        "//ast:ast",
        "//base:guarded",
        "//compiler/dispatch:table_header",
        "//error:log",
        "//ir:block_def",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:results",
        "//ir:scope_def",
        "//module:module",
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
        "//ast:ast_fwd",
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
    name = "dependent_data",
    deps = [
        ":verify_result",
        "//ast:ast_fwd",
        "//ast:expr_ptr",
        "//module:pending",
        "//ir:scope_def",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "module",
    deps = [
        ":constant_binding",
        ":dependent_data",
        "//ast:ast",
        "//ast:ast_fwd",
        "//backend:eval",
        "//base:ptr_span",
        "//ir:compiled_fn",
        "//module:module",
        "//type:type_fwd",
    ],
    test_deps = None,
)

cc_lib(
    name = "verify_result",
    deps = [],
    header_only = True,
    test_deps = None,
)
