package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "cc_component")

cc_lib_target(name = "type_fwd", impl_deps = None, test_deps = None)

cc_lib_target(
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
    test_deps = None,
)

cc_lib_target(name = "callable",
              intf_deps = [":type"],
              impl_deps = None,
              test_deps = None)
cc_group_target(
    name = "all_types",
    deps = [":concrete_types", ":generic_struct"])

cc_group_target(
    name = "concrete_types",
    deps = [
        ":array",
        ":enum",
        ":flags",
        ":function",
        ":jump",
        ":opaque",
        ":parameter_pack",
        ":pointer",
        ":primitive",
        ":struct",
        ":tuple",
        ":type",
        ":variant",
    ])

cc_group_target(
    name = "impl",
    deps = [
        ":array-impl",
        ":enum-impl",
        ":flags-impl",
        ":function-impl",
        ":generic_struct-impl",
        ":jump-impl",
        ":opaque-impl",
        ":parameter_pack-impl",
        ":pointer-impl",
        ":primitive-impl",
        ":struct-impl",
        ":tuple-impl",
        ":type-impl",
        ":variant-impl",
    ])

cc_lib_target(
    name = "cast",
    # TODO this should be runnable without all the ir, but for now it's
    # necessary. because scope drags in module. reduce the test deps when you're
    # done fixing this.
    cfgs = ["match", "compile"],
    impl_deps = [
        ":all_types",
        "@com_google_absl//absl/algorithm:container",
    ],
    test_deps = ["//:impl"],
)

cc_lib_target(
    name = "enum",
    intf_deps = [
        ":type",
        ":typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_lib_target(
    name = "flags",
    intf_deps = [
        ":type",
        ":typed_value",
        "//ir:values",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/random:distributions",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_lib_target(
    name = "function",
    intf_deps = [
        ":callable",
        "//core:fn_params",
        ":typed_value",
    ],
    impl_deps = ["//base:guarded"],
    test_deps = None,
)

cc_lib_target(
    name = "generic_struct",
    intf_deps = [
        ":callable",
        ":type",
        "//misc:module",
        "//core:scope",
    ],
    impl_deps = [],
    test_deps = None,
)

cc_lib_target(
    name = "jump",
    intf_deps = [
        ":type",
        ":function",
        "@com_google_absl//absl/types:span",
    ],
    impl_deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "opaque",
    intf_deps = [":type"],
    impl_deps = ["//base:debug"],
    test_deps = None,
)

cc_lib_target(
    name = "pointer",
    intf_deps = [":type"],
    impl_deps = [
        ":function",
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "parameter_pack",
    intf_deps = [":type"],
    impl_deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "primitive",
    textual_hdrs = ["primitive.xmacro.h"],
    intf_deps = [":type"],
    impl_deps = [
        ":array",
        ":pointer",
    ],
    test_deps = None,
)

cc_lib_target(
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
        "//base:guarded",
        "//core:arch",
        "//misc:module",
    ],
    test_deps = None,
)

cc_lib_target(
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
    test_deps = None,
)

cc_component(name = "typed_value", impl_deps = None, test_deps = None)

cc_lib_target(
    name = "type",
    textual_hdrs = ["primitive.xmacro.h"],
    intf_deps = [
        ":type_deps",
        "//base:debug",
        "//base:meta",
        "//base:tag",
        "//base:util",
        "//ir:addr",
        "//ir:block_def",
        "//ir:reg",
        "//ir:results",
        "//ir:values",
        "//core:arch",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        ":array",
        ":struct",
        ":tuple",
        ":variant",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "util",
    intf_deps = [":concrete_types"],
    impl_deps = None,
    test_deps = None)

cc_lib_target(
    name = "variant",
    intf_deps = [":type"],
    impl_deps = [
        "//base:guarded",
        "@com_google_absl//absl/algorithm:container",
    ],
    test_deps = None,
)

cc_library(
    name = "type_deps-compile",
    textual_hdrs = ["compile_type_dependencies.h"],
    deps = [dep + "-compile" for dep in [
        "//compiler:compiler",
        "//visitor:type_query",
        "//compiler:type_methods-xmacro",
    ]],
)

cc_library(
    name = "type_deps-match",
    textual_hdrs = ["match_type_dependencies.h"],
    deps = [dep + "-match" for dep in [
        "//compiler:compiler",
        "//visitor:type_query",
        "//match:type_methods-xmacro",
    ]],
)

cc_library(
    name = "type_deps-format",
    textual_hdrs = ["format_type_dependencies.h"],
    deps = [dep + "-format" for dep in [
        "//format:type_methods-xmacro",
    ]],
)
