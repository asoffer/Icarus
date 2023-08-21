package(default_visibility = ["//visibility:public"])

cc_library(
    name = "ir",
    hdrs = ["ir.h"],
    srcs = ["ir.cc"],
    deps = [
        "//parser:parse_tree",
        "//parser:parse_tree_node_kind_xmacro",
        "//type",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

