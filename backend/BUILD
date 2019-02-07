package(default_visibility = ["//visibility:public"])

# cc_library(
#     name = "emit",
#     hdrs = ["emit.h"],
#     deps = ["//base/container:vector"],
# )
# 
# cc_library(
#     name = "emit-impl",
#     srcs = ["emit.cc"],
#     deps = [":emit"],
# )

cc_library(
    name = "eval",
    hdrs = ["eval.h"],
    deps = [
        ":exec",
        "//base:debug",
        "//base:untyped_buffer",
        "//ir:val",
        "//misc:context",
    ],
)

cc_library(
    name = "eval-impl",
    srcs = ["eval.cc"],
    deps = [
        ":eval",
        "//misc:architecture",
        "//ast:expression",
        "//backend:exec",
        "//misc:context",
        "//ir:func",
        "//type:generic_struct",
        "//type:util",
    ],
)

cc_library(
    name = "exec",
    hdrs = ["exec.h"],
    srcs = ["exec.cc"],
    deps = [
        "//ast:block_literal",
        "//ast:expression",
        "//ast:function_literal",
        "//ast:scope_node",
        "//ast:struct_literal",
        "//base:util",
        "//error:log",
        "//ir:arguments",
        "//ir:func",
        "//misc:architecture",
        "//misc:module",
        "//type:generic_struct",
        "//type:incomplete_enum",
        "//type:incomplete_flags",
        "//type:util",
    ],
)

#write_obj.cc
