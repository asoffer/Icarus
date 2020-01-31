package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "match_expr",
    deps = [
        ":binding_node",
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
    ],
    test_deps = None,
)

cc_lib(
    name = "binding_id",
    deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
    ],
    test_deps = [],
)

cc_lib(
    name = "binding_node",
    deps = [
        ":binding_id",
        "//ast:expression",
        "//module:assign_scope",
    ],
    header_only = True,
    test_deps = None,
)

cc_binary(
    name = "match",
    srcs = ["main.cc"],
    defines = ["ICARUS_MATCHER"],
    deps = [
        ":binding_id",
        ":match_expr",
        "//ast:ast",
        "//base:macros",
        "//diagnostic/consumer:streaming",
        "//frontend:parse",
        "//frontend/source:file",
        "//frontend/source:file_name",
        "//init:cli",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
    ],
)
