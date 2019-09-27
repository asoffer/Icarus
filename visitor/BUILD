package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "cc_component", "icarus_ast_method")

icarus_ast_method(
    name = "xvisitors",
    deps = ["//ast/methods:dump-xmacro"],
    gen = False
)

# TODO rename ast_visitors
# TODO hack mirroring -impl, -test
cc_group_target(
    name = "visitors",
    deps = {
        "compile": [
            ":xvisitors-xmacro",
            ":assign_scope",
            ":dependent_decls",
            "//ast/methods:dump",
            ":extract_jumps",
            "//compiler:compiler",
        ],
        "format": [
            ":xvisitors-xmacro",
            "//ast/methods:dump",
            "//format:token_extractor",
        ],
        "match": [
            ":xvisitors-xmacro",
            ":assign_scope",
            ":dependent_decls",
            "//ast/methods:dump",
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
    deps = {
        "compile": [
            ":visitors",
            ":assign_scope-impl",
            ":dependent_decls-impl",
            "//ast/methods:dump-impl",
            ":extract_jumps-impl",
            "//compiler:compiler-impl",
        ],
        "format": [
            ":visitors",
            "//ast/methods:dump-impl",
            "//format:token_extractor-impl",
        ],
        "match": [
            ":visitors",
            ":assign_scope-impl",
            ":dependent_decls-impl",
            "//ast/methods:dump-impl",
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
