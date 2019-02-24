package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_lib(
    name = "impl",
    deps = [
        ":addr-impl",
        ":arguments-impl",
        ":builtin-impl",
        ":cmd-impl",
        ":components-impl",
        ":flags_val-impl",
        ":foreign-impl",
        ":func-impl",
        ":out_params-impl",
        ":phi-impl",
        ":results-impl",
        ":val-impl",
    ],
)

cc_target(
    name = "addr",
    intf_deps = ["//base:untyped_buffer"],
    impl_deps = ["//base:debug"],
)

cc_lib(
    name = "any_func",
    hdrs = ["any_func.h"],
    deps = [
        ":foreign",
        "//base:debug",
    ],
)

cc_target(
    name = "arguments",
    intf_deps = [
        ":register",
        ":results",
        "//base:untyped_buffer",
        "//type:function",
        "//type:generic_struct",
        "//type:util",
    ],
    impl_deps = [
        ":func",
        ":val",
        "//layout:arch",
    ],
)

cc_lib(
    name = "basic_block",
    hdrs = ["basic_block.h"],
    deps = [
        ":arguments",
        ":cmd",
        ":out_params",
        "//base:untyped_buffer",
    ],
)

cc_target(
    name = "builtin",
    textual_hdrs = ["builtin.xmacro.h"],
    impl_deps = [
        ":any_func",
        ":func",
        "//base:debug",
    ],
)

cc_target(
    name = "cmd",
    textual_hdrs = ["op.xmacro.h"],
    intf_deps = [
        ":val",
        "//ast:hashtag",
        "//base:untyped_buffer",
        "//misc:context",
    ],
    impl_deps = [
        ":func",
        ":phi",
        ":val",
        "//ast:struct_literal",
        "//layout:arch",
        "//type:typed_value",
        "//type:generic_struct",
    ],
)

cc_target(
    name = "components",
    intf_deps = [
        ":func",
        ":register",
    ],
    impl_deps = [],
)

cc_target(
    name = "flags_val",
    intf_deps = ["//base:strong_types"],
    impl_deps = ["//type:flags"],
)

cc_target(
    name = "foreign",
    impl_deps = [
        "//base:guarded",
        "//type:type",
    ],
)

cc_target(
    name = "func",
    intf_deps = [
        "//ast:fn_params",
        "//ast:function_literal",
        "//base:bag",
        "//ir:basic_block",
        "//property:property_map",
    ],
    impl_deps = [
        ":arguments",
        "//ast:function_literal",
        "//layout:arch",
        "//property:property",
        "//property:property_map",
        "//type:function",
        "//type:pointer",
    ],
)

cc_target(
    name = "out_params",
    intf_deps = [
        ":register",
    ],
)

cc_target(
    name = "phi",
    intf_deps = [
        ":func",
        ":val",
    ],
    impl_deps = [
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:struct",
    ],
)

cc_lib(
    name = "register",
    hdrs = ["register.h"],
    deps = ["//base:strong_types"],
)

cc_target(
    name = "results",
    intf_deps = [
        ":register",
        "//base:untyped_buffer",
    ],
    impl_deps = [":val"],
)

cc_test(
    name = "results_test",
    srcs = ["results_test.cc"],
    deps = [
        ":results-impl",
        "//test",
    ],
)

cc_target(
    name = "val",
    intf_deps = [
        ":addr",
        ":any_func",
        ":flags_val",
        ":register",
        "//base:strong_types",
        "//type:typed_value",
        "//type:util",
        "//type:interface",
    ],
    impl_deps = [
        "//ast:block_literal",
        "//ast:function_literal",
        "//ast:scope_literal",
        "//base:guarded",
        "//ir:func",
        "//layout:arch",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:struct",
    ],
)
