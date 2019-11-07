package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "cc_component", "icarus_method")

icarus_method(
    name = "type_methods",
    deps = [
        ":compiler-compile",
        "//visitor:type_query-compile",
        "//module:extract_defining_modules-xmacro",
    ],
    gen = False,
)

cc_lib_target(
    name = "type_dependencies",
    cfgs = ["compile"],
    textual_hdrs = ["type_dependencies.h"],
    intf_deps = [
        ":compiler",
        ":type_methods-xmacro",
        "//visitor:type_query",
        "//module:extract_defining_modules",
    ],
)

cc_lib_target(
    name = "compiler",
    cfgs = ["match", "compile"],
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

cc_lib_target(
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

cc_lib_target(
    name = "dependent_data",
    cfgs = ["match", "compile"],
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

cc_lib_target(
    name = "emit_assign",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
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

cc_lib_target(
    name = "emit_destroy",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
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

cc_lib_target(
    name = "emit_default_init",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
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

cc_lib_target(
    name = "emit_copy_init",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        "//ast:ast",
        "//ir:components",
        "//ir/cmd:misc",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_move_init",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        "//ast:ast",
        "//ir/cmd:misc",
        "//ir:components",
        "//type:type",
        "//type:typed_value",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "emit_ref",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
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

cc_lib_target(
    name = "emit_value",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        "//ast:ast",
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

cc_lib_target(
    name = "emit_print",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
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


cc_lib_target(
    name = "module",
    cfgs = ["match", "compile"],
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

cc_lib_target(
    name = "verify_type",
    cfgs = ["match", "compile"],
    intf_deps = None,
    impl_deps = [
        "//ast:ast",
        "//ast:overload_set",
        "//backend:eval",
        "//compiler/dispatch:dispatch",
        "//type:cast",
        "//type:generic_struct",
        "//type:jump",
        "//type:parameter_pack",
        "//type:type",
        "//type:util",
        "//visitor:extract_jumps",
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

cc_lib_target(
    name = "special_function",
    cfgs = ["match", "compile"],
    intf_deps = [],
    impl_deps = [
        "//ast:ast",
        "//ir:any_func",
        "//type:function",
        "//type:pointer",
        "//type:struct",
    ],
    test_deps = None,
)
