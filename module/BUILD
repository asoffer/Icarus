package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "icarus_method")

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
        "//module:assign_scope",
    ],
)

icarus_method(
    name = "dependent_decls",
    intf_deps = [
        "//ast:ast_fwd",
        "//base:graph",
    ],
    impl_deps = ["//ast:ast"],
)

icarus_method(
    name = "assign_scope",
    intf_deps = [
        "//ast:ast_fwd",
        "//core:scope",
    ],
    impl_deps = ["//ast:ast"],
)

icarus_method(
    name = "extract_defining_modules",
    intf_deps = [
        "//type:type_fwd",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = [
        ":module",
        "//type:concrete_types",
        "//type:generic_struct",
    ],
)
