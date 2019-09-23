package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "cc_component")

# TODO rename ast_visitors
# TODO hack mirroring -impl, -test
cc_group_target(
    name = "visitors",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = {
        "compile": [
            ":assign_scope",
            ":dependent_decls",
            ":dump_ast",
            ":extract_jumps",
            "//compiler:compiler",
        ],
        "format": [
            ":dump_ast",
            "//format:token_extractor",
        ],
        "match": [
            ":assign_scope",
            ":dependent_decls",
            ":dump_ast",
            ":extract_jumps",
            ":match",
            "//compiler:compiler",
        ],

    }
)

# TODO rename ast_visitors-impl
# TODO hack mirroring -impl, -test
cc_group_target(
    name = "visitors-impl",
    textual_hdrs = ["visitors.xmacro.h"],
    deps = {
        "compile": [
            ":assign_scope-impl",
            ":dependent_decls-impl",
            ":dump_ast-impl",
            ":extract_jumps-impl",
            "//compiler:compiler-impl",
        ],
        "format": [
            ":dump_ast-impl",
            "//format:token_extractor-impl",
        ],
        "match": [
            ":assign_scope-impl",
            ":dependent_decls-impl",
            ":dump_ast-impl",
            ":extract_jumps-impl",
            ":match-impl",
            "//compiler:compiler-impl",
        ],
    }
)

# TODO hack mirroring -impl, -test
cc_group_target(
    name = "type_visitors",
    textual_hdrs = ["type_visitors.xmacro.h"],
    deps = {
        "compile": [
            "//compiler:compiler",
            ":type_query",
        ],
        "match": [
            "//compiler:compiler",
            ":type_query",
        ]
    },
)
cc_group_target(
    cfgs = ["match", "compile"],
    name = "type_visitors-impl",
    textual_hdrs = ["type_visitors.xmacro.h"],
    deps = [
        ":type_query-impl",
    ])

cc_lib_target(
    name = "assign_scope",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:ast_fwd",
        "//core:scope",
    ],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)

cc_lib_target(
    name = "dump_ast",
    intf_deps = [
        "//ast:ast_fwd",
    ],
    impl_deps = [
        "//base:stringify",
        "//ast:ast",
        "//frontend:token",
        "//ir:results",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "dependent_decls",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//ast:ast_fwd",
        "//base:graph",
    ],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)

cc_lib_target(
    name = "extract_jumps",
    cfgs = ["match", "compile"],
    intf_deps = ["//ast:ast_fwd"],
    impl_deps = ["//ast:ast"],
    test_deps = None,
)


cc_lib_target(
    name = "match",
    cfgs = ["match"],
    intf_deps = [
        "//ast:ast_fwd",
    ],
    impl_deps = [
        ":match_state",
        "//match:binding_node",
        "//ast:ast",
    ],
    test_deps = None,
)

cc_lib_target(
    name = "match_state",
    intf_deps = ["//ast:ast_fwd"],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "type_query",
    cfgs = ["match", "compile"],
    intf_deps = ["//type:type_fwd"],
    impl_deps = [
        "//ast:ast",
        "//type:all_types",
        "@com_google_absl//absl/algorithm:container",
    ],
    test_deps = None,
)
