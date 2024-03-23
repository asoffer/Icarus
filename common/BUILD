package(default_visibility = ["//visibility:public"])

cc_library(
    name = "any_value",
    hdrs = ["any_value.h"],
    deps = [
        "//type",
        "@jasmin//jasmin/core:value",
    ],
)

cc_library(
    name = "debug",
    hdrs = ["debug.h"],
)

cc_library(
    name = "errno",
    hdrs = ["errno.h"],
)

cc_test(
    name = "errno_test",
    srcs = ["errno_test.cc"],
    deps = [
        ":errno",
        "@nth_cc//nth/test:main",
    ]
)

cc_library(
    name = "foreign_function",
    hdrs = ["foreign_function.h"],
    srcs = ["foreign_function.cc"],
    deps = [
        ":result",
        ":string_literal",
        ":strong_identifier_type",
        "//type:function",
        "@nth_cc//nth/container:flyweight_map",
        "@nth_cc//nth/container:flyweight_set",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/synchronization",
    ],
)

cc_library(
    name = "identifier",
    hdrs = ["identifier.h"],
    srcs = ["identifier.cc"],
    deps = [
        "//common/internal:constant_handle",
        "//common/internal:identifiers",
        "@nth_cc//nth/container:flyweight_set",
    ],
)

cc_library(
    name = "integer",
    hdrs = ["integer.h"],
    srcs = ["integer.cc"],
    deps = [
        "//common/constant:category",
        "//common/constant:entry",
        "//common/constant:manifest",
        "//common/internal:constant_handle",
        "//common/internal:integers",
        "@nth_cc//nth/numeric:integer",
        "@nth_cc//nth/container:flyweight_map",
    ],
)

cc_test(
    name = "integer_test",
    srcs = ["integer_test.cc"],
    deps = [
        ":integer",
        "@nth_cc//nth/test:main",
    ]
)

cc_library(
    name = "interface",
    hdrs = ["interface.h"],
    srcs = ["interface.cc"],
    deps = [
        ":strong_identifier_type",
        "@jasmin//jasmin/core:value",
    ],
)

cc_library(
    name = "module_id",
    hdrs = ["module_id.h"],
    deps = [
        "@jasmin//jasmin/core:value",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
    ],
)

cc_library(
    name = "module_map",
    hdrs = ["module_map.h"],
    srcs = ["module_map.cc"],
    deps = [
        ":module_id",
        "@nth_cc//nth/container:flyweight_set",
    ],
)

cc_library(
    name = "pattern",
    hdrs = ["pattern.h"],
    deps = [
        ":any_value",
        "@jasmin//jasmin/core:function",
    ],
)

cc_library(
    name = "string",
    hdrs = ["string.h"],
    deps = [
        "@nth_cc//nth/io:string_printer",
        "@nth_cc//nth/strings:interpolate",
        "@nth_cc//nth/strings/format:universal",
    ],
)

cc_library(
    name = "strong_identifier_type",
    hdrs = ["strong_identifier_type.h"],
)

cc_library(
    name = "strong_index_type",
    hdrs = ["strong_index_type.h"],
)


cc_library(
    name = "resources",
    hdrs = ["resources.h"],
    deps = [
        ":module_map",
    ],
)

cc_library(
    name = "result",
    hdrs = ["result.h"],
    deps = [
        "@nth_cc//nth/base:configuration",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/utility:early_exit",
    ],
)

cc_library(
    name = "slice",
    hdrs = ["slice.h"],
)

cc_library(
    name = "string_literal",
    hdrs = ["string_literal.h"],
    srcs = ["string_literal.cc"],
    deps = [
        "//common/constant:category",
        "//common/constant:entry",
        "//common/constant:manifest",
        "//common/internal:constant_handle",
        "//common/internal:strings",
    ],
)

cc_library(
    name = "to_bytes",
    hdrs = ["to_bytes.h"],
)
