package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
sources()

cc_lib(
    name = "impl",
    deps = [
        ":addr-impl",
        ":arguments-impl",
        ":basic_block-impl",
        ":builtin-impl",
        ":cmd-impl",
        ":compiled_fn-impl",
        ":components-impl",
        ":flags_val-impl",
        ":foreign-impl",
        ":out_params-impl",
        ":phi-impl",
        ":results-impl",
        ":str-impl",
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
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//core:arch",
    ],
)

cc_test(
    name = "arguments_test",
    srcs = ["arguments_test.cc"],
    deps = [
        "//:impl",
        "//test",
    ],
)

cc_target(
    name = "basic_block",
    intf_deps = [
        ":arguments",
        ":cmd",
        ":out_params",
        "//base:untyped_buffer",
    ],
)

cc_lib(
    name = "block",
    hdrs = ["block.h"],
)

cc_target(
    name = "builtin",
    textual_hdrs = ["builtin.xmacro.h"],
    impl_deps = [
        ":any_func",
        ":compiled_fn",
        "//base:debug",
    ],
)

cc_target(
    name = "cmd",
    textual_hdrs = ["op.xmacro.h"],
    intf_deps = [
        ":block",
        ":register",
        "//ast:hashtag",
        "//base:untyped_buffer",
        "//misc:context",
        "//type:util",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":compiled_fn",
        ":phi",
        "//ast:block_literal",
        "//ast:struct_literal",
        "//core:arch",
        "//type:typed_value",
        "//type:generic_struct",
    ],
)

cc_target(
    name = "components",
    intf_deps = [
        ":compiled_fn",
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
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_target(
    name = "compiled_fn",
    intf_deps = [
        "//ast:function_literal",
        "//base:bag",
        "//core:fn_params",
        "//ir:basic_block",
        "//property:property_map",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        ":arguments",
        "//ast:function_literal",
        "//core:arch",
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
        ":compiled_fn",
        "@com_google_absl//absl/container:flat_hash_map",
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
        "//core:bytes",
        "//base:untyped_buffer",
    ],
    impl_deps = [
        "//base:untyped_buffer-impl",
    ],
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
    name = "str",
    intf_deps = [":addr"],
    impl_deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)
