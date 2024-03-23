package(default_visibility = ["//visibility:public"])

cc_library(
    name = "declaration",
    hdrs = ["declaration.h"],
    srcs = ["declaration.cc"],
    deps = [
        "//common:identifier",
        "//common:string",
        "//diagnostics/consumer",
        "//parse:tree",
        "@nth_cc//nth/container:stack",
    ],
)

cc_library(
    name = "dependent_modules",
    hdrs = ["dependent_modules.h"],
    srcs = ["dependent_modules.cc"],
    deps = [
        ":module",
        "//common:module_id",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "deserialize",
    hdrs = ["deserialize.h"],
    deps = [
        ":dependent_modules",
        ":function",
        ":module",
        "//common/constant:manifest",
        "//common:foreign_function",
        "//common:result",
        "//common:to_bytes",
        "@jasmin//jasmin/core:function_registry",
        "@nth_cc//nth/base:attributes",
        "@nth_cc//nth/container:black_hole",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/debug/log",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/reader:string",
    ],
)

cc_library(
    name = "emit",
    hdrs = ["emit.h"],
    srcs = ["emit.cc"],
    deps = [
        ":dependent_modules",
        ":lexical_scope",
        ":local_storage",
        ":module",
        ":scope",
        ":serialize",
        "//common:debug",
        "//common:identifier",
        "//common:module_id",
        "//common/language:primitive_types",
        "//common:resources",
        "//parse:declaration",
        "//parse:node_index",
        "//parse:node_xmacro",
        "//parse:tree",
        "//type",
        "//type:qualified_type",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:btree",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/debug/log",
        "@nth_cc//nth/container:interval_map",
        "@nth_cc//nth/container:stack",
    ],
)

cc_library(
    name = "function",
    hdrs = [
        "function.h",
        "global_function_registry.h",
    ],
    srcs = [
        "function.cc",
        "global_function_registry.cc",
    ],
    deps = [
        ":function_id",
        ":program_arguments",
        "//common:foreign_function",
        "//common:identifier",
        "//common:integer",
        "//common:interface",
        "//common:module_id",
        "//common:pattern",
        "//common:resources",
        "//common:string_literal",
        "//type",
        "//type:function",
        "//type:opaque",
        "//type:parameters",
        "//type:primitive",
        "//type:pointer",
        "//type:refinement",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/container:node_hash_map",
        "@com_google_absl//absl/strings",
        "@jasmin//jasmin/core:function",
        "@jasmin//jasmin/core:function_registry",
        "@jasmin//jasmin/core:instruction",
        "@jasmin//jasmin/core:input",
        "@jasmin//jasmin/core:output",
        "@jasmin//jasmin/core:program_fragment",
        "@jasmin//jasmin/core:value",
        "@jasmin//jasmin/instructions:arithmetic",
        "@jasmin//jasmin/instructions:bool",
        "@jasmin//jasmin/instructions:common",
        "@jasmin//jasmin/instructions:compare",
        "@jasmin//jasmin/instructions:stack",
        "@nth_cc//nth/container:flyweight_map",
    ],
)

cc_library(
    name = "function_id",
    hdrs = ["function_id.h"],
    deps = [
        "//common:module_id",
        "@nth_cc//nth/strings:interpolate",
    ],
)

cc_library(
    name = "ir",
    hdrs = ["ir.h"],
    srcs = ["ir.cc"],
    deps = [
        ":emit",
        ":lexical_scope",
        ":type_stack",
        "//common:debug",
        "//common:module_id",
        "//common:string",
        "//diagnostics/consumer",
        "//parse:node_index",
        "//parse:node_xmacro",
        "//parse:tree",
        "//type",
        "//type:cast",
        "//type:dependent",
        "@com_google_absl//absl/container:flat_hash_map",
        "@jasmin//jasmin/core:function",
        "@jasmin//jasmin/core:value",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/debug/log",
    ],
)


cc_library(
    name = "local_storage",
    hdrs = ["local_storage.h"],
    deps = [
        ":lexical_scope",
        "//parse:node_index",
        "//type",
        "//type:alignment",
        "//type:byte_width",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/container:interval",
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":function",
        ":scope",
        "//common:identifier",
        "//type",
        "@com_google_absl//absl/container:inlined_vector",
        "@com_google_absl//absl/container:flat_hash_map",
        "@jasmin//jasmin/core:value",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/debug/log",
    ],
)

cc_library(
    name = "program_arguments",
    hdrs = ["program_arguments.h"],
    srcs = ["program_arguments.cc"],
    deps = [
        "//common:slice",
        "@com_google_absl//absl/synchronization",
        "@nth_cc//nth/utility:no_destructor",
    ],
)


cc_library(
    name = "lexical_scope",
    hdrs = ["lexical_scope.h"],
    srcs = ["lexical_scope.cc"],
    deps = [
        "//common:identifier",
        "//common:strong_identifier_type",
        "//parse:node_index",
        "//type",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/utility:iterator_range",
    ]
)

cc_library(
    name = "scope",
    hdrs = ["scope.h"],
    srcs = ["scope.cc"],
    deps = [
        ":function",
    ]
)

cc_library(
    name = "serialize",
    hdrs = ["serialize.h"],
    deps = [
        ":function",
        ":module",
        "//common/constant:manifest",
        "//common:foreign_function",
        "//common:result",
        "//common:to_bytes",
        "@jasmin//jasmin/core:function_registry",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/debug/log",
        "@nth_cc//nth/io/serialize",
        "@nth_cc//nth/io/writer:string",
    ],
)

cc_library(
    name = "type_stack",
    hdrs = ["type_stack.h"],
    srcs = ["type_stack.cc"],
    deps = [
        "//type",
        "//type:qualified_type",
        "@nth_cc//nth/debug",
    ],
)
