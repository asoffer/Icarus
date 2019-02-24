package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_target")

# cc_target(
#     name = "emit",
# )

cc_target(
    name = "eval",
    intf_deps = [
        ":exec",
        "//base:debug",
        "//base:untyped_buffer",
        "//ir:val",
        "//misc:context",
    ],
    impl_deps = [
        "//layout:arch",
        "//ast:expression",
        "//backend:exec",
        "//ir:func",
        "//type:generic_struct",
        "//type:util",
    ],
)

cc_target(
    name = "exec",
    intf_deps = [
        "//base:untyped_buffer",
        "//ir:basic_block",
        "//ir:cmd",
        "//ir:val",
    ],
    impl_deps = [
        ":eval",
        "//ast:block_literal",
        "//ast:expression",
        "//ast:function_literal",
        "//ast:scope_node",
        "//ast:struct_literal",
        "//base:util",
        "//error:log",
        "//ir:arguments",
        "//ir:func",
        "//layout:arch",
        "//misc:module",
        "//type:generic_struct",
        "//type:incomplete_enum",
        "//type:incomplete_flags",
        "//type:util",
    ],
)

#write_obj.cc
