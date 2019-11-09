package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component")

cc_component(
    name = "compiler",
    intf_deps = [
        ":constant_binding",
        ":module",
        ":verify_result",
        "//ast:visitor",
        "//base:debug",
        "//base:guarded",
        "//base:move_func",
        "//base:tag",
        "//error:log",
        "//compiler:dependent_data",
        "//module:module",
        "//ir:results",
        "//ir:builder",
        "//type:type_fwd",
        "//type:visitor",
        "@com_google_absl//absl/container:node_hash_map",
    ],
    impl_deps = [
        "//ast:ast",
        "//frontend:parse",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:components",
        "//ir/cmd:call",
        "//ir/cmd:print",
        "//ir/cmd:basic",
        "//module:module",
        "//type:type",
        "//type:typed_value",
        ":emit_assign-impl",
        ":emit_destroy-impl",
        ":emit_copy_init-impl",
        ":emit_default_init-impl",
        ":emit_move_init-impl",
        ":emit_ref-impl",
        ":emit_value-impl",
        ":emit_print-impl",
        ":special_function-impl",
        ":verify_type-impl",
    ],
    test_deps = None,
)

cc_component(
    name = "extract_jumps",
    intf_deps = ["//ast:visitor"],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)

cc_component(
    name = "constant_binding",
    intf_deps = [
        "//ast:ast_fwd",
        "//base:untyped_buffer",
        "//core:bytes",
        "//ir:results",
        "//type:type_fwd",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//core:arch",
        "//type:type",
    ],
    test_deps = None,
)

cc_component(
    name = "dependent_data",
    intf_deps = [
        ":verify_result",
        "//ast:dispatch_table",
        "//ast:ast_fwd",
        "//ast:expr_ptr",
        "//module:pending",
        "//ir:scope_def",
    ],
    impl_deps = None,
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

cc_component(
    name = "emit_move_init",
    intf_deps = None,
    impl_deps = [
        ":compiler",
        "//ast:ast",
        "//ir/cmd:misc",
        "//ir:components",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_component(
    name = "emit_ref",
    intf_deps = None,
    impl_deps = [
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

cc_component(
    name = "emit_value",
    intf_deps = None,
    impl_deps = [
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

cc_component(
    name = "emit_print",
    intf_deps = None,
    impl_deps = [
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


cc_component(
    name = "module",
    intf_deps = [
        ":constant_binding",
        "//ast:ast_fwd",
        "//base:ptr_span",
        "//compiler:dependent_data",
        "//module:module",
        "//type:type_fwd",
    ],
    impl_deps = [
        ":compiler",
        "//ast:ast",
        "//backend:eval",
        "//ir:compiled_fn",
    ],
    test_deps = None,
)

cc_component(
    name = "verify_type",
    intf_deps = None,
    impl_deps = [
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

cc_component(
    name = "verify_result",
    intf_deps = [],
    impl_deps = None,
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
