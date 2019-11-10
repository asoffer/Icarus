package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component", "cc_lib")

cc_lib(
    name = "compiler",
    deps = [
        ":data",
        ":verify_result",
        "//ast:ast",
        "//ast:visitor",
        "//base:debug",
        "//base:guarded",
        "//base:move_func",
        "//base:tag",
        "//compiler:dependent_data",
        "//error:log",
        "//frontend:parse",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:results",
        "//module:module",
        "//type:type",
        "//type:type_fwd",
        "//type:typed_value",
        "//type:visitor",
        "@com_google_absl//absl/container:node_hash_map",
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
        "//error:log",
        "//ir:block_def",
        "//ir:builder",
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
        "//ast:dispatch_table",
        "//ast:ast_fwd",
        "//ast:expr_ptr",
        "//module:pending",
        "//ir:scope_def",
    ],
    header_only = True,
    test_deps = None,
)

cc_component(
    name = "emit_assign",
    intf_deps = None,
    impl_deps = [
        ":compiler",
        ":special_function",
        "//ast:ast",
        "//base:permutation",
        "//ir:builder",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:store",
        "//ir:compiled_fn",
        "//ir:components",
        "//type:type",
        "//type:typed_value",
        "@com_google_absl//absl/random",
    ],
    test_deps = None,
)

cc_component(
    name = "emit_destroy",
    intf_deps = None,
    impl_deps = [
        ":compiler",
        ":special_function",
        "//ast:ast",
        "//base:permutation",
        "//ir:builder",
        "//ir/cmd:call",
        "//ir/cmd:misc",
        "//ir:compiled_fn",
        "//ir:components",
        "//type:type",
        "//type:typed_value",
        "@com_google_absl//absl/random",
    ],
    test_deps = None,
)

cc_component(
    name = "emit_default_init",
    intf_deps = None,
    impl_deps = [
        ":compiler",
        "//ast:ast",
        "//base:permutation",
        "//ir/cmd:misc",
        "//ir/cmd:store",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:components",
        "//type:type",
        "//type:typed_value",
        "@com_google_absl//absl/random",
    ],
    test_deps = None,
)

cc_component(
    name = "emit_copy_init",
    intf_deps = None,
    impl_deps = [
        ":compiler",
        "//ast:ast",
        "//ir:components",
        "//ir/cmd:misc",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib(
    name = "emit_move_init",
    deps = [
        ":compiler",
        "//ast:ast",
        "//ir/cmd:misc",
        "//ir:components",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib(
    name = "emit_ref",
    deps = [
        ":compiler",
        "//ast:ast",
        "//backend:eval",
        "//ir:addr",
        "//ir/cmd:cast",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir:components",
        "//ir:reg",
        "//ir:str",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib(
    name = "emit_value",
    deps = [
        ":compiler",
        "//ast:ast",
        "//ast/methods:dump",
        "//ast/scope:scope",
        "//ast/scope:fn",
        "//backend:eval",
        "//base:guarded",
        "//module:pending",
        "//ir:builder",
        "//ir:builtin_ir",
        "//ir:components",
        "//ir:results",
        "//ir/cmd:basic",
        "//ir/cmd:call",
        "//ir/cmd:cast",
        "//ir/cmd:load",
        "//ir/cmd:misc",
        "//ir/cmd:phi",
        "//ir/cmd:return",
        "//ir/cmd:scope-impl",
        "//ir/cmd:store",
        "//ir/cmd:types-impl",
        "//type:jump",
        "//type:generic_struct",
        "//type:pointer",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib(
    name = "emit_print",
    deps = [
        ":compiler",
        "//ast:ast",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:components",
        "//ir/cmd:call",
        "//ir/cmd:print",
        "//ir/cmd:basic",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)


cc_lib(
    name = "module",
    deps = [
        ":compiler",
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
    name = "verify_type",
    deps = [
        ":extract_jumps",
        "//ast:ast",
        "//ast/methods:dump",
        "//ast:overload_set",
        "//backend:eval",
        "//compiler/dispatch:dispatch",
        "//type:cast",
        "//type:generic_struct",
        "//type:jump",
        "//type:parameter_pack",
        "//type:type",
        "//type:util",
        "@com_google_absl//absl/algorithm:container",
    ],
    test_deps = None,
)

cc_lib(
    name = "verify_result",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_component(
    name = "special_function",
    intf_deps = [],
    impl_deps = [
        ":compiler",
        "//ast:ast",
        "//ir:any_func",
        "//type:function",
        "//type:pointer",
        "//type:struct",
    ],
    test_deps = None,
)
