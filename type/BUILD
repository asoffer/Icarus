package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "array",
    hdrs = ["array.h"],
    deps = [
        "//base:lazy",
        "//type:type",
    ],
)

cc_lib(
    name = "array-impl",
    srcs = ["array.cc"],
    deps = [
        ":array",
        ":function",
        ":pointer",
        "//ast:fn_params",
        "//base:guarded",
        "//ir:any_func",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//misc:architecture",
        "//misc:context",
        "//misc:module",
    ],
)

cc_lib(
    name = "callable",
    hdrs = ["callable.h"],
    deps = [":type"],
)

cc_lib(
    name = "cast",
    hdrs = ["cast.h"],
)

cc_lib(
    name = "cast-impl",
    srcs = ["cast.cc"],
    deps = [
        ":cast",
        ":array",
        ":enum",
        ":flags",
        ":function",
        ":generic_struct",
        ":interface",
        ":opaque",
        ":pointer",
        ":primitive",
        ":struct",
        ":tuple",
        ":variant",
    ],
)

#TODO remove me.
cc_lib(
    name = "comparator-impl",
    srcs = ["comparator.cc"],
    deps = [
        ":array",
        ":enum",
        ":flags",
        ":function",
        ":generic_struct",
        ":interface",
        ":opaque",
        ":pointer",
        ":primitive",
        ":struct",
        ":tuple",
        ":util",
        ":variant",
    ],
)

cc_lib(
    name = "enum",
    hdrs = ["enum.h"],
    deps = [
        ":type",
        ":typed_value",
        "//base/container:unordered_map",
    ],
)

cc_lib(
    name = "enum-impl",
    srcs = ["enum.cc"],
    deps = [
        ":enum",
        "//ir:addr",
        "//ir:cmd",
        "//ir:register",
        "//ir:val",
    ],
)

cc_lib(
    name = "flags",
    hdrs = ["flags.h"],
    deps = [
        ":type",
        ":typed_value",
        "//base/container:unordered_map",
    ],
)

cc_lib(
    name = "flags-impl",
    srcs = ["flags.cc"],
    deps = [
        ":flags",
        "//ir:addr",
        "//ir:cmd",
        "//ir:flags_val",
        "//ir:register",
        "//ir:val",
    ],
)

cc_lib(
    name = "function",
    hdrs = ["function.h"],
    deps = [":callable"]
)

cc_lib(
    name = "function-impl",
    srcs = ["function.cc"],
    deps = [
        "//base/container:map",
        "//base:guarded",
        "//ir:cmd",
        "//ir:val",
    ],
)

cc_lib(
    name = "generic_struct",
    hdrs = ["generic_struct.h"],
    srcs = ["generic_struct.cc"],
    deps = [
        ":callable",
        ":type",
        "//base/container:vector",
        "//misc:module",
        "//misc:scope",
    ],
)

cc_lib(
    name = "incomplete_enum",
    hdrs = ["incomplete_enum.h"],
    srcs = ["incomplete_enum.cc"],
    deps = [
        ":enum",
        "//base/container:vector",
        "//ir:register",
        "//misc:module",
    ],
)

cc_lib(
    name = "incomplete_flags",
    hdrs = ["incomplete_flags.h"],
    srcs = ["incomplete_flags.cc"],
    deps = [
        ":flags",
        "//base/container:vector",
        "//ir:register",
        "//misc:module",
    ],

)

cc_lib(
    name = "interface",
    hdrs = ["interface.h"],
    deps = [
        ":type",
        "//misc:scope",
    ],
)

cc_lib(
    name = "interface-impl",
    srcs = ["interface.cc"],
    deps = [
        ":function",
        ":interface",
        ":pointer",
        "//base/container:map",
        "//base:guarded",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//misc:architecture",
        "//misc:context",
        "//misc:module",
    ],
)

#TODO llvm.cc

cc_lib(
    name = "opaque",
    hdrs = ["opaque.h"],
    deps = [":type"],
)

cc_lib(
    name = "opaque-impl",
    srcs = ["opaque.cc"],
    deps = [
        ":opaque",
        "//base:debug",
        "//ir:val",
    ],
)

cc_lib(
    name = "pointer",
    hdrs = ["pointer.h"],
    deps = [":type"],
)

cc_lib(
    name = "pointer-impl",
    srcs = ["pointer.cc"],
    deps = [
        ":function",
        "//base/container:unordered_map",
        "//base:guarded",
        "//ir:cmd",
    ],
)

cc_lib(
    name = "prepare_argument-impl",
    srcs = ["prepare_argument.cc"],
    deps = [
        ":util",
        ":generic_struct",
        "//ir:components",
        "//ir:func",
    ],
)

cc_lib(
    name = "primitive",
    hdrs = ["primitive.h"],
    textual_hdrs = ["primitive.xmacro.h"],
    deps = [":type"],
)

cc_lib(
    name = "primitive-impl",
    textual_hdrs = ["primitive.xmacro.h"],
    srcs = ["primitive.cc"],
    deps = [
        ":primitive",
        "//ir:cmd"
    ],
)

cc_lib(
    name = "struct",
    hdrs = ["struct.h"],
    deps = [
        ":type",
        "//ast:hashtag",
        "//base/container:vector",
        "//base:lazy",
        "//ir:any_func",
        "//misc:scope",
    ]
)

cc_lib(
    name = "struct-impl",
    srcs = ["struct.cc"],
    deps = [
        ":function",
        ":pointer",
        "//ast:declaration",
        "//ast:struct_literal",
        "//base:guarded",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//misc:architecture",
        "//misc:context",
        "//misc:module",
    ],
)

cc_lib(
    name = "tuple",
    hdrs = ["tuple.h"],
    deps = [
        ":type",
        "//base:lazy",
        "//ir:any_func",
    ],
)

cc_lib(
    name = "tuple-impl",
    srcs = ["tuple.cc"],
    deps = [
        ":function",
        ":pointer",
        ":tuple",
        "//base/container:map",
        "//base:guarded",
        "//ir:arguments",
        "//ir:components",
        "//misc:architecture",
        "//misc:context",
        "//misc:module",
    ],
)

cc_lib(
    name = "typed_value",
    hdrs = ["typed_value.h"],
    deps = [":type"],
)

cc_lib(
    name = "type",
    hdrs = ["type.h"],
    textual_hdrs = ["primitive.xmacro.h"],
    deps = [
        "//base:debug",
        "//base:util",
        "//ir:addr",
        "//ir:register",
    ],
)

cc_lib(
    name = "type-impl",
    srcs = ["type.cc"],
    deps = [
        ":type",
        "//ast:declaration",
        "//ast:struct_literal",
        "//base/container:map",
        "//base/container:unordered_map",
        "//base:guarded",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
        "//ir:phi",
        "//misc:architecture",
        "//misc:context",
        "//misc:module",
    ],
)

cc_lib(
    name = "util",
    hdrs = ["util.h"],
    deps = [
        ":array",
        ":enum",
        ":flags",
        ":function",
        ":interface",
        ":opaque",
        ":pointer",
        ":primitive",
        ":struct",
        ":tuple",
        ":type",
        ":variant",
        ":typed_value",
    ],
)

cc_lib(
    name = "variant",
    hdrs = ["variant.h"],
    deps = [":type"],
)

cc_lib(
    name = "variant-impl",
    srcs = ["variant.cc"],
    deps = [
        ":type",
        "//base/container:map",
        "//base:guarded",
        "//misc:context",
        "//ir:arguments",
        "//ir:components",
        "//ir:func",
    ],
)

cc_lib(
    name = "impl",
    deps = [
        ":array-impl",
        ":cast-impl",
        ":comparator-impl",
        ":enum-impl",
        ":flags-impl",
        ":function-impl",
        ":interface-impl",
        ":opaque-impl",
        ":pointer-impl",
        ":prepare_argument-impl",
        ":primitive-impl",
        ":struct-impl",
        ":tuple-impl",
        ":type-impl",
        ":variant-impl",
    ],
)
