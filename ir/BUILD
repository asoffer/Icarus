package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "addr",
    hdrs = ["addr.h"],
    srcs = ["addr.cc"],
    deps = [
        "//base:debug",
        "//base:types",
        "//base:untyped_buffer",
    ],
)

cc_lib(
    name = "any_func",
    hdrs = ["any_func.h"],
    deps = [
        ":foreign",
        "//base:debug",
        "//type:function",
    ],
)

cc_lib(
    name = "arguments",
    hdrs = ["arguments.h"],
    deps = [
        ":register",
        "//base/container:vector",
        "//base:untyped_buffer",
        "//type:function",
        "//type:generic_struct",
        "//type:util",
    ],
)

cc_lib(
    name = "arguments-impl",
    srcs = ["arguments.cc"],
    deps = [
        ":arguments",
        ":func",
        ":val",
        "//misc:architecture",
    ],
)

cc_lib(
    name = "basic_block",
    hdrs = ["basic_block.h"],
    deps = [
        ":arguments",
        ":cmd",
        ":out_params",
        "//base/container:vector",
        "//base:untyped_buffer",
    ],
)

cc_lib(
    name = "cmd",
    hdrs = ["cmd.h"],
    textual_hdrs = ["op.xmacro.h"],
    deps = [
        ":val",
        "//ast:hashtag",
        "//base:untyped_buffer",
        "//misc:context",
    ],
)

cc_lib(
    name = "cmd-impl",
    srcs = ["cmd.cc"],
    deps = [
        ":cmd",
        ":func",
        ":phi",
        ":val",
        "//ast:struct_literal",
        "//base/container:vector",
        "//misc:architecture",
        "//type:typed_value",
        "//type:generic_struct",
    ],
)

cc_lib(
    name = "components",
    hdrs = ["components.h"],
    deps = [
        ":cmd",
        ":func",
        ":phi",
        ":register",
        "//base/container:tuple",
    ],
)

cc_lib(
    name = "flags_val",
    hdrs = ["flags_val.h"],
    srcs = ["flags_val.cc"],
    deps = [
        "//base:strong_types",
        "//type:flags",
    ],
)

cc_lib(
    name = "foreign",
    hdrs = ["foreign.h"],
    srcs = ["foreign.cc"],
    deps = [
        "//base/container:unordered_map",
        "//base:guarded",
        "//type:type",
    ],
)

cc_lib(
    name = "func",
    hdrs = ["func.h"],
    deps = [
        "//ast:fn_params",
        "//ast:function_literal",
        "//base/container:bag",
        "//base/container:unordered_map",
        "//base/container:vector",
        "//ir:basic_block",
        "//property:property_map",
    ],
)

cc_lib(
    name = "func-impl",
    srcs = ["func.cc"],
    deps = [
        ":arguments",
        ":func",
        "//ast:function_literal",
        "//misc:architecture",
        "//property:property",
        "//property:property_map",
        "//type:function",
        "//type:pointer",
    ],
)

cc_lib(
    name = "out_params",
    hdrs = ["out_params.h"],
    srcs = ["out_params.cc"],
    deps = [
        ":register",
        "//base/container:vector",
    ],
)

cc_lib(
    name = "phi",
    hdrs = ["phi.h"],
    srcs = ["phi.cc"],
    deps = [
        ":func",
        ":val",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:struct",
    ],
)

cc_lib(
    name = "register",
    hdrs = ["register.h"],
    deps = [
        "//base/container:vector",
        "//base:strong_types",
        "//base:types",
    ],
)

cc_lib(
    name = "val",
    hdrs = ["val.h"],
    deps = [
        ":addr",
        ":any_func",
        ":flags_val",
        ":register",
        "//base:strong_types",
        "//base:types",
        "//type:flags",
        "//type:type",
        "//type:typed_value",
        "//type:util",
    ],
)

cc_lib(
    name = "val-impl",
    srcs = ["val.cc"],
    deps = [
        ":val",
        "//ast:block_literal",
        "//ast:function_literal",
        "//ast:scope_literal",
        "//base:guarded",
        "//ir:func",
        "//misc:architecture",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:struct",
    ],
)

cc_lib(
    name = "impl",
    deps = [
        ":arguments-impl",
        ":cmd-impl",
        ":func-impl",
        ":val-impl",
    ],
)
