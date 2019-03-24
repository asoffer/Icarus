package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_target(
    name = "array",
    intf_deps = [
        "//base:lazy",
        "//type:type",
    ],
    impl_deps = [
        ":function",
        ":pointer",
        "//base:guarded",
        "//base:tuple",
        "//core:fn_params",
        "//ir:any_func",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//layout:arch",
        "//misc:context",
        "//misc:module",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_target(
    name = "block",
    intf_deps = [":callable"],
)

cc_lib(
    name = "callable",
    hdrs = ["callable.h"],
    deps = [":type"],
)

cc_lib(
    name = "all_types",
    deps = [
        ":concrete_types",
        ":generic_struct",
    ],
)

cc_lib(
    name = "concrete_types",
    deps = [
        ":array",
        ":block",
        ":enum",
        ":flags",
        ":function",
        ":incomplete_enum",
        ":incomplete_flags",
        ":opaque",
        ":pointer",
        ":primitive",
        ":struct",
        ":tuple",
        ":type",
        ":variant",
    ],
)

cc_lib(
    name = "impl",
    deps = [
        ":array-impl",
        ":block-impl",
        ":cast-impl",
        ":enum-impl",
        ":flags-impl",
        ":function-impl",
        ":generic_struct-impl",
        ":incomplete_enum-impl",
        ":incomplete_flags-impl",
        ":interface-impl",
        ":opaque-impl",
        ":pointer-impl",
        ":primitive-impl",
        ":struct-impl",
        ":tuple-impl",
        ":type-impl",
        ":variant-impl",
    ],
)
cc_target(
    name = "cast",
    impl_deps = [
        ":all_types",
        "@com_google_absl//absl/algorithm:container",
    ],
)

cc_test(
    name = "cast_test",
    srcs = ["cast_test.cc"],
    deps = [
        "//:impl",
        "//test",
    ],
)

cc_target(
    name = "enum",
    intf_deps = [
        ":type",
        ":typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//ir:addr",
        "//ir:cmd",
        "//ir:register",
        "//ir:val",
    ],
)

cc_target(
    name = "flags",
    intf_deps = [
        ":type",
        ":typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//ir:addr",
        "//ir:cmd",
        "//ir:flags_val",
        "//ir:register",
        "//ir:val",
    ],
)

cc_target(
    name = "function",
    intf_deps = [
        ":callable",
        "//core:fn_params",
    ],
    impl_deps = [
        "//base:guarded",
        "//ir:cmd",
        "//ir:val",
    ],
)

cc_target(
    name = "generic_struct",
    intf_deps = [
        ":callable",
        ":type",
        "//misc:module",
        "//core:scope",
    ],
)

cc_target(
    name = "incomplete_enum",
    intf_deps = [
        "//ir:register",
    ],
    impl_deps = [
        ":enum",
        "//misc:module",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_target(
    name = "incomplete_flags",
    intf_deps = [
        "//ir:register",
    ],
    impl_deps = [
        ":flags",
        "//misc:module",
    ],

)

cc_target(
    name = "interface",
    intf_deps = [
        ":type",
        "//core:scope",
    ],
    impl_deps = [
        ":function",
        ":pointer",
        "//base:guarded",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//layout:arch",
        "//misc:context",
        "//misc:module",
    ],
)

#TODO llvm.cc

cc_target(
    name = "opaque",
    intf_deps = [":type"],
    impl_deps = [
        "//base:debug",
        "//ir:val",
    ],
)

cc_target(
    name = "pointer",
    intf_deps = [":type"],
    impl_deps = [
        ":function",
        "//base:guarded",
        "//ir:cmd",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_target(
    name = "primitive",
    textual_hdrs = ["primitive.xmacro.h"],
    intf_deps = [":type"],
    impl_deps = ["//ir:cmd"],
)

cc_target(
    name = "struct",
    intf_deps = [
        ":type",
        "//ast:hashtag",
        "//base:lazy",
        "//ir:any_func",
        "//core:scope",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":function",
        ":pointer",
        "//ast:declaration",
        "//ast:struct_literal",
        "//base:guarded",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//layout:arch",
        "//misc:context",
        "//misc:module",
    ],
)

cc_target(
    name = "tuple",
    intf_deps = [
        ":type",
        "//base:lazy",
        "//ir:any_func",
    ],
    impl_deps = [
        ":function",
        ":pointer",
        "//base:guarded",
        "//ir:arguments",
        "//ir:components",
        "//layout:arch",
        "//misc:context",
        "//misc:module",
    ],
)

cc_lib(
    name = "typed_value",
    hdrs = ["typed_value.h"],
    deps = [":type"],
)

cc_target(
    name = "type",
    textual_hdrs = ["primitive.xmacro.h"],
    intf_deps = [
        "//base:debug",
        "//base:util",
        "//ir:addr",
        "//ir:register",
        "//ir:results",
        "//layout:arch",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        ":generic_struct",
        "//ast:declaration",
        "//ast:struct_literal",
        "//base:guarded",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//layout:arch",
        "//misc:context",
        "//misc:module",
    ],
)

cc_lib(
    name = "util",
    hdrs = ["util.h"],
    deps = [":concrete_types"],
)

cc_target(
    name = "variant",
    intf_deps = [":type"],
    impl_deps = [
        "//base:guarded",
        "//misc:context",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "@com_google_absl//absl/algorithm:container",
    ],
)
