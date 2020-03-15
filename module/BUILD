package(default_visibility = ["//visibility:public"])

cc_library(
    name = "pending",
    hdrs = ["pending.h"],
    srcs = ["pending.cc"],
    deps = [
        ":module",
        "//base:debug",
        "//base:expected",
        "//base:graph",
        "//base:macros",
        "//diagnostic/consumer:streaming",
        "//frontend/source:source",
        "//frontend/source:file",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        "//ast:ast",
        "//ast:ast_fwd",
        "//ast/scope:module",
        "//base:cast",
        "//base:macros",
        "//base:ptr_span",
        "//diagnostic/consumer",
        "//frontend:parse",
        "//frontend/source",
        "@com_google_absl//absl/algorithm:container",
    ],
)
