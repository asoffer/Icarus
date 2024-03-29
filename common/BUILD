package(default_visibility = ["//visibility:public"])

cc_library(
    name = "any_value",
    hdrs = ["any_value.h"],
    deps = [
        ":type",
        "@jasmin//jasmin/core:value",
    ],
)

cc_library(
    name = "constants",
    hdrs = [
        "constants.h",
        "identifier.h",
        "integer.h",
        "string_literal.h",
    ],
    srcs = ["constants.cc"],
    deps = [
        ":any_value",
        ":type",
        ":pattern",
        ":result",
        ":strong_identifier_type",
        ":to_bytes",
        "//common/internal:component",
        "//common/internal:constant_handle",
        "//common/language:primitive_types",
        "//common/language:type_kind",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/container:flyweight_map",
        "@nth_cc//nth/container:flyweight_set",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
        "@nth_cc//nth/numeric:integer",
        "@nth_cc//nth/utility:bytes",
    ],
)

cc_test(
    name = "constants_test",
    srcs = ["constants_test.cc"],
    deps = [
        ":constants",
        "@nth_cc//nth/test:main",
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

alias(name = "integer", actual = ":constants")
alias(name = "identifier", actual = ":constants")
alias(name = "string_literal", actual = ":constants")

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
        "@nth_cc//nth/utility:early_exit",
    ],
)

cc_library(
    name = "slice",
    hdrs = ["slice.h"],
)

cc_library(
    name = "to_bytes",
    hdrs = ["to_bytes.h"],
)

cc_library(
    name = "type",
    hdrs = ["type.h"],
    deps = [
        "//common/internal:constant_handle",
        "//common/language:type_kind",
    ],
)

