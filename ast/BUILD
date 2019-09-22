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
        "//base:graph",
        "//base:ptr_span",
        "//core:builtin",
        "//core:fn_args",
        "//core:fn_params",
        "//core:ordered_fn_args",
        "//core:scope",
        "//frontend:operators",
        "//ir:block",
        "//ir:reg",
        "//type:primitive",
    ],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(name = "expression",
              intf_deps = [
                  ":expr_ptr",
                  ":hashtag",
                  ":node",
              ],
              impl_deps = None,
              test_deps = None)

cc_lib_target(
    name = "node",
    intf_deps = [
        "//base:util",
        "//visitor:visitors",
        "//frontend/source:range",
        "//ir:results",
    ],
)

cc_lib_target(
    name = "expr_ptr",
    intf_deps = [],
    impl_deps = None,
    test_deps = None,
)

cc_lib_target(
    name = "dispatch_table",
    cfgs = ["match", "compile"],
    intf_deps = [
        "//core:fn_args",
        "//base:expected",
        "//ir:basic_block",
        "//ir:block",
        "//ir:results",
        "@com_google_absl//absl/container:flat_hash_map",
        "//visitor:verify_result",
    ],
    impl_deps = [
        ":ast",
        ":overload_set",
        "//backend:eval",
        "//ir:block",
        "//ir:builder",
        "//ir/cmd:call",
        "//ir/cmd:phi",
        "//ir/cmd:store",
        "//ir:components",
        "//ir:compiled_fn",
        "//misc:context",
        "//misc:module",
        "//core:scope",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
        "//visitor:traditional_compilation",
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
        "//base:bag",
        "//base:ptr_span",
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
