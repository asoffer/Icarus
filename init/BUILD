package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")
sources()

cc_lib_target(
    name = "cli",
    intf_deps = [
        "//base:util",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = ["//base:debug"],
    test_deps = None,
)
cc_lib_target(name = "signal", impl_deps = [], test_deps = None)
