package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_component", "icarus_method")

icarus_method(
    name = "dump",
    intf_deps = ["//ast:ast_fwd"],
    impl_deps = [
        "//base:stringify",
        "//ast:ast",
        "//ast/methods:dump-xmacro",
        "//frontend:token",
        "//ir:results",
        "@com_google_absl//absl/strings",
    ],
)
