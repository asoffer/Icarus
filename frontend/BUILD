package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

# TODO handle match
cc_lib(
    name = "lex",
    deps = [
        ":lexeme",
        ":numbers",
        "//ast:ast",
        "//core:builtin",
        "//error:log",
        "//frontend/source:cursor",
        "//ir:block_def",
        "//ir:results",
        "//ir:str",
        "//type:primitive",
        "//type:type",
        "//type:util",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "lexeme",
    deps = [":operators", ":syntax", ":tag"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "numbers",
    deps = ["//base:expected"],
    test_deps = None,
)

cc_lib(
    name = "operators",
    textual_hdrs = ["operators.xmacro.h"],
    deps = [":tag", "//base:debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "syntax",
    textual_hdrs = ["syntax.xmacro.h"],
    deps = [":tag", "//base:debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "parse_rule",
    deps = [
        ":tag",
        "//ast:node",
        "@com_google_absl//absl/types:span",
    ],
    test_deps = None,
)

cc_lib(
    name = "parse",
    deps = [
        ":lex",
        ":tagged_node",
        ":token",
        ":parse_rule",
        "//ast:ast",
        "//ast:ast_fwd",
        "//base:guarded",
        "//error:log",
        "//frontend/source:source",
        "//ast/methods:dump",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)

cc_lib(
    name = "tag",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "tagged_node",
    deps = [":tag", ":lexeme"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "token",
    deps = [
        ":operators",
        "//ast:node",
        "//base:debug",
    ],
    header_only = True,
    test_deps = None,
)
