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
        "//base:log",
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
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "binding_node",
    hdrs = ["binding_node.h"],
    deps = [
        ":binding_id",
        "//ast:expression",
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
        "//base:no_destructor",
        "//diagnostic/consumer:streaming",
        "//frontend:parse",
        "//frontend/source:file",
        "//frontend/source:file_name",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/flags:flag",
        "@com_google_absl//absl/flags:parse",
        "@com_google_absl//absl/strings",
    ],
)
