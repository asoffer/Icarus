package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target", "sources")
sources()

cc_group_target(
    name = "lex",
    hdrs = ["lex.h"],
    deps = [
        ":lexeme",
        ":source",
        ":text_span",
        "//error:log",
    ],
)

COMMON_LEX_IMPL_TARGETS = [
    ":lex",
    ":numbers-impl",
    "//ast:builtin_fn",
    "//ast:identifier",
    "//ast:terminal",
    "//ir:block",
    "//ir:builtin",
    "//ir:results",
    "//ir:str",
    "//type:primitive",
    "//type:type",
    "//type:util",
    "@com_google_absl//absl/container:flat_hash_map",
]

cc_group_target(
    name = "lex-impl",
    srcs = ["lex.cc"],
    deps = {
        "compile": COMMON_LEX_IMPL_TARGETS,
        "format": COMMON_LEX_IMPL_TARGETS,
        "match": COMMON_LEX_IMPL_TARGETS + [
            "//match:binding_id",
            "//match:binding_node",
        ],
    },
)

cc_lib_target(name = "lexeme",
              intf_deps = [":operators", ":syntax", ":tag"],
              impl_deps = None,
              test_deps = None)

cc_lib_target(name = "numbers",
              intf_deps = ["//base:expected"],
              impl_deps = [],
              test_deps = None)

cc_lib_target(name = "operators",
              textual_hdrs = ["operators.xmacro.h"],
              intf_deps = [":tag", "//base:debug"],
              impl_deps = None,
              test_deps = None)

cc_lib_target(
    name = "syntax",
    textual_hdrs = ["syntax.xmacro.h"],
    intf_deps = [":tag", "//base:debug"],
    impl_deps = None,
    test_deps = None)

cc_lib_target(
    name = "parse",
    intf_deps = None,
    impl_deps = [
        ":lex",
        ":source",
        ":tagged_node",
        ":token",
        "//ast:ast",
        "//base:guarded",
        "//error:log-impl",
        "//misc:module",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib_target(name = "source",
              intf_deps = ["//base:expected"],
              impl_deps = [],
              test_deps = [],
              test_data = [
                  "testdata/empty_file.txt",
                  "testdata/one_line_file.txt",
                  "testdata/multi_line_file.txt",
              ])

cc_lib_target(name = "tag", impl_deps = None, test_deps = None)

cc_lib_target(name = "tagged_node",
              intf_deps = [":tag", ":lexeme"],
              impl_deps = None,
              test_deps = None)

cc_lib_target(name = "text_span",
              intf_deps = ["//base:debug", "//base:interval"],
              impl_deps = None,
              test_deps = None)

cc_lib_target(name = "token",
              intf_deps = [":operators", "//ast:node"],
              impl_deps = None,
              test_deps = None)
