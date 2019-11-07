package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component")

cc_component(
    name = "dump",
    intf_deps = ["//ast:ast_fwd"],
    impl_deps = [
        "//base:stringify",
        "//ast:ast",
        "//frontend:token",
        "//ir:results",
        "@com_google_absl//absl/strings",
    ],
)
