package(default_visibility = ["//visibility:public"])

cc_library(
    name = "array",
    hdrs = ["array.h"],
    deps = [
        "//base:lazy",
        "//type:type",
    ],
)

cc_library(
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

cc_library(
    name = "callable",
    hdrs = ["callable.h"],
    deps = [":type"],
)

cc_library(
    name = "cast",
    hdrs = ["cast.h"],
)

cc_library(
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

#TODO comparator.cc
cc_library(
    name = "enum",
    hdrs = ["enum.h"],
    deps = [
        ":type",
        ":typed_value",
        "//base/container:unordered_map",
    ],
)

cc_library(
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

cc_library(
    name = "flags",
    hdrs = ["flags.h"],
    deps = [
        ":type",
        ":typed_value",
        "//base/container:unordered_map",
    ],
)

cc_library(
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

cc_library(
    name = "function",
    hdrs = ["function.h"],
    deps = [":callable"]
)

cc_library(
    name = "function-impl",
    srcs = ["function.cc"],
    deps = [
        "//base/container:map",
        "//base:guarded",
        "//ir:cmd",
        "//ir:val",
    ],
)

cc_library(
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

cc_library(
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

cc_library(
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

cc_library(
    name = "interface",
    hdrs = ["interface.h"],
    deps = [
        ":type",
        "//misc:scope",
    ],
)

cc_library(
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

cc_library(
    name = "opaque",
    hdrs = ["opaque.h"],
    deps = [":type"],
)

cc_library(
    name = "opaque-impl",
    srcs = ["opaque.cc"],
    deps = [
        ":opaque",
        "//base:debug",
        "//ir:val",
    ],
)

cc_library(
    name = "pointer",
    hdrs = ["pointer.h"],
    deps = [":type"],
)

cc_library(
    name = "pointer-impl",
    srcs = ["pointer.cc"],
    deps = [
        ":function",
        "//base/container:unordered_map",
        "//base:guarded",
        "//ir:cmd",
    ],
)

#TODO prepare_argument.cc

cc_library(
    name = "primitive",
    hdrs = ["primitive.h"],
    textual_hdrs = ["primitive.xmacro.h"],
    deps = [":type"],
)

cc_library(
    name = "primitive-impl",
    textual_hdrs = ["primitive.xmacro.h"],
    srcs = ["primitive.cc"],
    deps = [
        ":primitive",
        "//ir:cmd"
    ],
)

cc_library(
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

cc_library(
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

cc_library(
    name = "tuple",
    hdrs = ["tuple.h"],
    deps = [
        ":type",
        "//base:lazy",
        "//ir:any_func",
    ],
)

cc_library(
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

cc_library(
    name = "typed_value",
    hdrs = ["typed_value.h"],
    deps = [":type"],
)

cc_library(
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
cc_library(
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

cc_library(
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

cc_library(
    name = "variant",
    hdrs = ["variant.h"],
    deps = [":type"],
)

cc_library(
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
