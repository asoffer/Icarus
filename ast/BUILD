package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")

sources()

# TODO this is a hack
cc_library(name = "node_xmacro-compile", textual_hdrs = ["node.xmacro.h"])
cc_library(name = "node_xmacro-format", textual_hdrs = ["node.xmacro.h"])
cc_library(name = "node_xmacro-match", textual_hdrs = ["node.xmacro.h"])

cc_lib_target(
    name = "ast_fwd",
    intf_deps = [":node_xmacro"],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "ast",
    intf_deps = [
        ":expression",
        ":node",
        ":node_span",
        "//base:graph",
        "//core:builtin",
        "//core:fn_args",
        "//core:fn_params",
        "//core:ordered_fn_args",
        "//core:scope",
        "//frontend:operators",
        "//ir:block",
        "//type:primitive",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(name = "expression",
              intf_deps = [":hashtag", ":node"],
              impl_deps = None,
              test_deps = None)

cc_lib_target(
    name = "node",
    intf_deps = [
        "//base:util",
        "//visitor:visitors",
        "//frontend:text_span",
        "//ir:results",
    ],
)

cc_lib_target(name = "node_span", intf_deps = [], impl_deps = None, test_deps = None)

cc_lib_target(
    name = "dispatch_table",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":overload_set",
        "//core:fn_args",
        "//base:expected",
        "//ir:block",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        ":ast",
        "//backend:eval",
        "//ir:block",
        "//ir:cmd",
        "//ir:components",
        "//ir:compiled_fn",
        "//ir:phi",
        "//misc:context",
        "//misc:module",
        "//core:scope",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
    ],
    test_deps = [
        "//:impl",
        "//test:util",
        "//type:pointer",
        "//type:variant",
    ],
)

cc_lib_target(name = "hashtag")

cc_lib_target(
    name = "overload_set",
    cfgs = ["match", "compile"],
    intf_deps = [
        ":node_span",
        "//base:bag",
        "//type:typed_value",
        "//type:function",
    ],
    impl_deps = [
        ":ast",
        "//misc:context",
        "//core:scope",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    test_deps = None,
)
