package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "cc_component")

cc_lib_target(
    name = "pending",
    intf_deps = [
        "//base:expected",
        "//frontend/source:source",
    ],
    impl_deps = [
        ":module",
        "//base:debug",
        "//base:graph",
        "//base:macros",
        "//frontend/source:file",
        "@com_google_absl//absl/container:node_hash_set",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)

cc_lib_target(
    name = "module",
    intf_deps = [
        ":pending",
        "//base:ptr_span",
        "//core:scope",
        "//error:log",
        "@com_google_absl//absl/algorithm:container",
    ],
    impl_deps = [
        "//ast:ast",
        "//visitor:assign_scope",
    ],
)
