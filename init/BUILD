package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_target", "sources")
sources()

cc_target(
    name = "cli",
    intf_deps = [
        "//base:util",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = ["//base:debug"],
)
cc_target(name = "signal")
