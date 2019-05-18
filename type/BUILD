package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target", "sources")
sources()

cc_lib(
    name = "type-fwd",
    hdrs = ["type_fwd.h"],
)

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
        "//core:arch",
        "//misc:module",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
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
        ":interface",
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
    impl_deps = [],
)

cc_target(
    name = "flags",
    intf_deps = [
        ":type",
        ":typed_value",
        "//ir:flags_val",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [],
)

cc_target(
    name = "function",
    intf_deps = [
        ":callable",
        "//core:fn_params",
    ],
    impl_deps = ["//base:guarded"],
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
        "//ir:flags_val",
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
        "//core:arch",
        "//misc:module",
    ],
)

#TODO llvm.cc

cc_target(
    name = "opaque",
    intf_deps = [":type"],
    impl_deps = ["//base:debug"],
)

cc_target(
    name = "pointer",
    intf_deps = [":type"],
    impl_deps = [
        ":function",
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_target(
    name = "primitive",
    textual_hdrs = ["primitive.xmacro.h"],
    intf_deps = [":type"],
    impl_deps = [
        ":array",
        ":pointer",
    ],
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
        "//core:arch",
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
        "//base:permutation",
        "//core:arch",
        "//misc:module",
    ],
)

cc_lib(
    name = "typed_value",
    hdrs = ["typed_value.h"],
)

cc_target(
    name = "type",
    textual_hdrs = ["primitive.xmacro.h"],
    intf_deps = [
        "//base:debug",
        "//base:util",
        "//ir:addr",
        "//ir:block",
        "//ir:register",
        "//ir:results",
        "//core:arch",
        "@com_google_absl//absl/container:flat_hash_set",
        "//visitor:type_visitors",
    ],
    impl_deps = [
        ":array",
        ":struct",
        ":tuple",
        ":variant",
    ],
)

cc_lib(
    name = "util",
    hdrs = ["util.h"],
    deps = [
        ":concrete_types",
        ":interface",
    ],
)

cc_target(
    name = "variant",
    intf_deps = [":type"],
    impl_deps = [
        "//base:guarded",
        "@com_google_absl//absl/algorithm:container",
    ],
)
