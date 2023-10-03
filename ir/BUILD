package(default_visibility = ["//visibility:public"])

cc_library(
    name = "builtin_module",
    hdrs = ["builtin_module.h"],
    srcs = ["builtin_module.cc"],
    deps = [
        ":global_function_registry",
        ":module",
        ":module_id",
        "//lexer:token_buffer",
        "@asoffer_nth//nth/utility:no_destructor",
    ],
)

cc_library(
    name = "dependent_modules",
    hdrs = ["dependent_modules.h"],
    srcs = ["dependent_modules.cc"],
    deps = [
        ":module",
        ":module_id",
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
        ":module",
        ":module_cc_proto",
        "//lexer:token_buffer",
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
        ":module",
        ":module_id",
        "//parser:parse_tree",
        "//parser:parse_tree_node_kind_xmacro",
        "//type",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
        "@asoffer_nth//nth/container:interval_set",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:btree",
    ],
)

cc_library(
    name = "function",
    hdrs = ["function.h"],
    deps = [
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin:instruction",
        "@asoffer_jasmin//jasmin/instructions:core",
        "@asoffer_jasmin//jasmin:value",
    ],
)

cc_library(
    name = "function_id",
    hdrs = ["function_id.h"],
    deps = [
        "@asoffer_nth//nth/strings:interpolate",
    ],
)

cc_library(
    name = "global_function_registry",
    hdrs = ["global_function_registry.h"],
    deps = [
        ":function",
        ":function_id",
        ":module_id",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "ir",
    hdrs = ["ir.h"],
    srcs = ["ir.cc"],
    deps = [
        ":emit",
        ":module_id",
        "//common:string",
        "//diagnostics/consumer",
        "//parser:parse_tree",
        "//parser:parse_tree_node_kind_xmacro",
        "//type",
        "@asoffer_jasmin//jasmin:value_stack",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "module_id",
    hdrs = ["module_id.h"],
    deps = [
        "@asoffer_jasmin//jasmin:value",
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":function",
        ":global_function_registry",
        "//type",
        "@asoffer_jasmin//jasmin:value",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
        "@com_google_absl//absl/container:inlined_vector",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

proto_library(
    name = "module_proto",
    srcs = ["module.proto"],
    deps = [],
)

cc_proto_library(
    name = "module_cc_proto",
    visibility = ["//visibility:public"],
    deps = [":module_proto"],
)

cc_library(
    name = "serialize",
    hdrs = ["serialize.h"],
    srcs = ["serialize.cc"],
    deps = [
        ":global_function_registry",
        ":module",
        ":module_cc_proto",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/debug/log",
    ],
)
