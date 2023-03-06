package(default_visibility = ["//visibility:public"])

cc_library(
    name = "execute",
    hdrs = ["execute.h"],
    srcs = ["execute.cc"],
    deps = [
        ":function",
        ":implementation",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_jasmin//jasmin:execution_state",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:value_stack",
    ]
)

cc_library(
    name = "function",
    hdrs = ["function.h"],
    srcs = ["function.cc"],
    deps = [
        ":implementation",
        ":instructions",
        "//core/type_system:type",
        "//serialization:foreign_symbol_map",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:value",
    ]
)


cc_library(
    name = "implementation",
    hdrs = ["implementation.h"],
    # TODO visibility = ["//visibility:private"],
    deps = [
        "//semantic_analysis:instruction_set",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:value",
    ]
)

cc_library(
    name = "instructions",
    hdrs = ["instructions.h"],
    deps = [
        "@asoffer_jasmin//jasmin:instruction",
        "@asoffer_jasmin//jasmin:value_stack",
    ],
)
