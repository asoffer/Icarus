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
        "//type",
        "@asoffer_nth//nth/utility:no_destructor",
    ],
)

cc_library(
    name = "dependent_modules",
    hdrs = ["dependent_modules.h"],
    srcs = ["dependent_modules.cc"],
    deps = [
        ":module",
        "//common:module_id",
        "@asoffer_nth//nth/debug",
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
        "@asoffer_jasmin//jasmin/core:metadata",
        "@asoffer_nth//nth/base:attributes",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
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
        "@asoffer_jasmin//jasmin/instructions:arithmetic",
        "@asoffer_jasmin//jasmin/core:execute",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
        "@asoffer_nth//nth/container:interval_map",
        "@asoffer_nth//nth/container:stack",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:btree",
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
        "//common:module_id",
        "//common:resources",
        "//common:interface",
        "//type:basic",
        "//type:function",
        "//type:opaque",
        "//type:parameters",
        "//type:primitive",
        "//type:pointer",
        "@asoffer_jasmin//jasmin/core:function",
        "@asoffer_jasmin//jasmin/core:instruction",
        "@asoffer_jasmin//jasmin/core:value",
        "@asoffer_jasmin//jasmin/instructions:arithmetic",
        "@asoffer_jasmin//jasmin/instructions:bool",
        "@asoffer_jasmin//jasmin/instructions:common",
        "@asoffer_jasmin//jasmin/instructions:compare",
        "@asoffer_jasmin//jasmin/instructions:stack",
        "@asoffer_nth//nth/container:flyweight_map",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "function_id",
    hdrs = ["function_id.h"],
    deps = [
        "//common:module_id",
        "@asoffer_nth//nth/strings:interpolate",
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
        "@asoffer_jasmin//jasmin/core:execute",
        "@asoffer_jasmin//jasmin/core:value",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
        "@com_google_absl//absl/container:flat_hash_map",
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
        "@asoffer_nth//nth/container:interval",
        "@com_google_absl//absl/container:flat_hash_map",
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
        "@asoffer_jasmin//jasmin/core:value",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
        "@com_google_absl//absl/container:inlined_vector",
        "@com_google_absl//absl/container:flat_hash_map",
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
        "@asoffer_nth//nth/utility:no_destructor",
        "@com_google_absl//absl/synchronization",
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
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/utility:iterator_range",
        "@com_google_absl//absl/container:flat_hash_map",
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
        "@asoffer_jasmin//jasmin/core:metadata",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
    ],
)

cc_library(
    name = "type_erased_value",
    hdrs = ["type_erased_value.h"],
    deps = [
        "//type:basic",
    ],
)

cc_library(
    name = "type_stack",
    hdrs = ["type_stack.h"],
    srcs = ["type_stack.cc"],
    deps = [
        "//type",
        "@asoffer_nth//nth/debug",
    ],
)
