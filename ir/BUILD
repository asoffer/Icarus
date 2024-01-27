package(default_visibility = ["//visibility:public"])

cc_library(
    name = "builtin_module",
    hdrs = ["builtin_module.h"],
    srcs = ["builtin_module.cc"],
    deps = [
        ":function",
        ":module",
        "//common:identifier",
        "//common:module_id",
        "//common:pattern",
        "//type",
        "@nth_cc//nth/utility:no_destructor",
    ],
)

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
    srcs = ["deserialize.cc"],
    deps = [
        ":builtin_module",
        ":dependent_modules",
        ":function",
        ":module",
        ":module_cc_proto",
        "//type:deserialize",
        "@jasmin//jasmin/serialize:deserialize",
        "@jasmin//jasmin/serialize:string_reader",
        "@nth_cc//nth/base:attributes",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/debug/log",
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
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:btree",
        "@jasmin//jasmin/instructions:arithmetic",
        "@jasmin//jasmin/core:function",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/debug/log",
        "@nth_cc//nth/container:interval_map",
        "@nth_cc//nth/container:stack",
    ],
)

cc_library(
    name = "function",
    hdrs = [
        "foreign_function.h",
        "function.h",
        "global_function_registry.h",
    ],
    srcs = [
        "foreign_function.cc",
        "function.cc",
        "global_function_registry.cc",
    ],
    deps = [
        ":function_id",
        ":program_arguments",
        "//common:identifier",
        "//common:integer",
        "//common:interface",
        "//common:module_id",
        "//common:pattern",
        "//common:resources",
        "//common:string_literal",
        "//type:basic",
        "//type:function",
        "//type:opaque",
        "//type:parameters",
        "//type:primitive",
        "//type:pointer",
        "//type:refinement",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/container:node_hash_map",
        "@jasmin//jasmin/core:function",
        "@jasmin//jasmin/core:instruction",
        "@jasmin//jasmin/core:input",
        "@jasmin//jasmin/core:output",
        "@jasmin//jasmin/core:program",
        "@jasmin//jasmin/core:value",
        "@jasmin//jasmin/instructions:arithmetic",
        "@jasmin//jasmin/instructions:bool",
        "@jasmin//jasmin/instructions:common",
        "@jasmin//jasmin/instructions:compare",
        "@jasmin//jasmin/instructions:stack",
        "@jasmin//jasmin/serialize:writer",
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

proto_library(
    name = "module_proto",
    srcs = ["module.proto"],
    deps = [
        "//type:type_system_proto",
    ],
)

cc_proto_library(
    name = "module_cc_proto",
    visibility = ["//visibility:public"],
    deps = [":module_proto"],
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
    srcs = ["serialize.cc"],
    deps = [
        ":function",
        ":module",
        ":module_cc_proto",
        "//common:resources",
        "//type:serialize",
        "@jasmin//jasmin/serialize",
        "@jasmin//jasmin/serialize:string_writer",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/debug/log",
    ],
)

cc_library(
    name = "type_stack",
    hdrs = ["type_stack.h"],
    srcs = ["type_stack.cc"],
    deps = [
        "//type",
        "@nth_cc//nth/debug",
    ],
)
