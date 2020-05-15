package(default_visibility = ["//visibility:public"])

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast/scope:module",
        "//base:cast",
        "//base:debug",
        "//base:expected",
        "//base:graph",
        "//base:guarded",
        "//base:macros",
        "//base:ptr_span",
        "//diagnostic/consumer",
        "//diagnostic/consumer:streaming",
        "//frontend:parse",
        "//frontend/source",
        "//frontend/source:file",
        "//frontend/source:shared",
        "@com_google_absl//absl/algorithm:container",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/container:node_hash_map",
        "@com_google_absl//absl/synchronization",
    ],
)
