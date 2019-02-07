package(default_visibility = ["//visibility:public"])

cc_library(
    name = "addr",
    hdrs = ["addr.h"],
    srcs = ["addr.cc"],
    deps = [
        "//base:debug",
        "//base:types",
        "//base:untyped_buffer",
    ],
)

cc_library(
    name = "any_func",
    hdrs = ["any_func.h"],
    deps = [
        ":foreign",
        "//base:debug",
        "//type:function",
    ],
)

cc_library(
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

cc_library(
    name = "arguments-impl",
    srcs = ["arguments.cc"],
    deps = [
        ":arguments",
        ":func",
        ":val",
        "//misc:architecture",
    ],
)

cc_library(
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

cc_library(
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

cc_library(
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

cc_library(
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

cc_library(
    name = "flags_val",
    hdrs = ["flags_val.h"],
    srcs = ["flags_val.cc"],
    deps = [
        "//base:strong_types",
        "//type:flags",
    ],
)

cc_library(
    name = "foreign",
    hdrs = ["foreign.h"],
    srcs = ["foreign.cc"],
    deps = [
        "//base/container:unordered_map",
        "//base:guarded",
        "//type:type",
    ],
)

cc_library(
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

cc_library(
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

cc_library(
    name = "out_params",
    hdrs = ["out_params.h"],
    srcs = ["out_params.cc"],
    deps = [
        ":register",
        "//base/container:vector",
    ],
)

cc_library(
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

cc_library(
    name = "register",
    hdrs = ["register.h"],
    deps = [
        "//base/container:vector",
        "//base:strong_types",
        "//base:types",
    ],
)

cc_library(
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
