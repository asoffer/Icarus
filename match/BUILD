package(default_visibility = ["//visibility:public"])

cc_library(
    name = "match_expr",
    hdrs = ["match_expr.h"],
    srcs = ["match_expr.cc"],
    deps = [
        ":binding_node",
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast:visitor",
    ],
)

cc_library(
    name = "binding_id",
    hdrs = ["binding_id.h"],
    srcs = ["binding_id.cc"],
    deps = [
        "//base:guarded",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "binding_id_test",
    srcs = ["binding_id_test.cc"],
    deps = [
        ":binding_id",
        "//test",
    ],
)

cc_library(
    name = "binding_node",
    hdrs = ["binding_node.h"],
    deps = [
        ":binding_id",
        "//ast:expression",
        "//module:assign_scope",
    ],
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
