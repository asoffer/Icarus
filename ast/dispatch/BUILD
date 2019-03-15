package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib", "cc_target")

cc_lib(
    name = "dispatch",
    deps = [
        ":table",
        ":arg_resolution",
    ],
)

cc_lib(
    name = "impl",
    deps = [
        ":table-impl",
        ":arg_resolution-impl",
    ],
)

cc_target(
   name = "arg_resolution",
   intf_deps = [
       ":call_obstruction",
       "//ast:declaration",
       "//ast:expression",
       "//core:fn_args",
       "//core:fn_params",
       "//base:expected",
       "//type:typed_value",
       "//type:tuple",
   ],
   impl_deps = [
       "//backend:eval",
       "//ir:results",
       "//misc:context",
       "//type:cast",
   ],
)

cc_lib(
    name = "call_obstruction",
    hdrs = ["call_obstruction.h"],
    deps = [
        "//base:util",
        "//type:type",
    ],
)

cc_target(
    name = "table",
    intf_deps = [
        ":arg_resolution",
        "//ast:bound_constants",
        "//ast:overload_set",
        "//core:fn_args",
        "//base:expected",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    impl_deps = [
        "//ast:call",
        "//ast:builtin_fn",
        "//ast:block_literal",
        "//ast:function_literal",
        "//ast:match_declaration",
        "//backend:eval",
        "//misc:context",
        "//ir:cmd",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//misc:module",
        "//core:scope",
        "//type:cast",
        "//type:function",
        "//type:generic_struct",
        "//type:pointer",
        "//type:tuple",
        "//type:variant",
    ],
)
