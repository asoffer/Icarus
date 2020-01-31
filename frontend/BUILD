package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parse_rule",
    hdrs = ["parse_rule.h"],
    srcs = ["parse_rule.cc"],
    deps = [
        "//ast:node",
        "//diagnostic/consumer",
        "//frontend/lex:tag",
        "@com_google_absl//absl/types:span",
    ],
    test_deps = None,
)

cc_library(
    name = "parse",
    hdrs = ["parse.h"],
    srcs = ["parse.cc"],
    deps = [
        ":parse_rule",
        "//ast:ast",
        "//ast:ast_fwd",
        "//base:guarded",
        "//diagnostic/consumer",
        "//frontend/lex:lex",
        "//frontend/lex:tagged_node",
        "//frontend/lex:token",
        "//frontend/source:source",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    test_deps = None,
)
