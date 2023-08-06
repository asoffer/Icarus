package(default_visibility = ["//visibility:public"])

cc_library(
    name = "argument_slice",
    hdrs = ["argument_slice.h"],
    deps = [],
)

cc_library(
    name = "execute",
    hdrs = ["execute.h"],
    srcs = ["execute.cc"],
    deps = [
        ":argument_slice",
        ":function",
        "//data_types:integer",
        "//semantic_analysis:type_system",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_jasmin//jasmin:execution_state",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:value_stack",
    ]
)

cc_library(
    name = "function",
    hdrs = [
        "function.h",
        "function_table.h"
    ],
    srcs = [
        "function.cc",
        "function_table.cc",
        "implementation.h",
        "instructions.h",
        "instructions.cc",
    ],
    deps = [
        ":argument_slice",
        ":immediate_values",
        "//core/type_system",
        "//core/type_system:type",
        "//data_types:addr",
        "//data_types:fn",
        "//data_types:integer",
        "//module:global_function_map",
        "//module:unique_id",
        "//semantic_analysis:type_system",
        "//serialization:foreign_symbol_map",
        "//serialization:function_index",
        "//serialization:function_table_proto",
        "//serialization:read_only_data",
        "@asoffer_jasmin//jasmin:debug",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:instruction",
        "@asoffer_jasmin//jasmin/instructions:arithmetic",
        "@asoffer_jasmin//jasmin/instructions:bool",
        "@asoffer_jasmin//jasmin/instructions:compare",
        "@asoffer_jasmin//jasmin/instructions:core",
        "@asoffer_jasmin//jasmin/instructions:stack",
        "@asoffer_jasmin//jasmin:serialization",
        "@asoffer_jasmin//jasmin:value",
        "@asoffer_jasmin//jasmin:value_stack",
        "@asoffer_nth//nth/debug",
        "@com_google_absl//absl/container:btree",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/numeric:int128",
    ]
)


cc_library(
    name = "immediate_values",
    hdrs = ["immediate_values.h"],
    visibility = ["//visibility:private"],
    deps = [],
)

cc_library(
    name = "serialization",
    hdrs = ["serialization.h"],
    srcs = ["serialization.cc"],
    deps = [
        ":function",
        "//module:unique_id",
        "//serialization:foreign_symbol_map",
        "//serialization:function_proto",
        "//serialization:module",
        "//serialization:read_only_data",
    ],
)
