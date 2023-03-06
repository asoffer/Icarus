package(default_visibility = ["//visibility:public"])

cc_library(
    name = "execute",
    hdrs = ["execute.h"],
    srcs = ["execute.cc"],
    deps = [
        ":function",
        ":implementation",
        "//data_types:integer",
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
        ":immediate_values",
        ":implementation",
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
        ":instructions",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:value",
    ]
)

cc_library(
    name = "immediate_values",
    hdrs = ["immediate_values.h"],
    visibility = ["//visibility:private"],
    deps = [],
)

cc_library(
    name = "instructions",
    hdrs = ["instructions.h"],
    srcs = ["instructions.cc"],
    deps = [
        ":immediate_values",
        "//semantic_analysis:type_system",
        "//core/type_system",
        "//data_types:integer",
        "//data_types:fn",
        "//module:global_function_map",
        "//module:global_module_map",
        "//serialization:foreign_symbol_map",
        "//serialization:function_table",
        "//serialization:module_index",
        "//serialization:read_only_data",
        "@asoffer_nth//nth/numeric:integer",
        "@asoffer_jasmin//jasmin:debug",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:instruction",
        "@asoffer_jasmin//jasmin/instructions:arithmetic",
        "@asoffer_jasmin//jasmin/instructions:bool",
        "@asoffer_jasmin//jasmin/instructions:compare",
        "@asoffer_jasmin//jasmin/instructions:core",
        "@asoffer_jasmin//jasmin/instructions:stack",
        "@asoffer_jasmin//jasmin:serialization",
        "@asoffer_jasmin//jasmin:value_stack",

    ],
)
