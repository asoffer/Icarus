package(default_visibility = ["//common:__subpackages__"])

cc_library(
    name = "constant_handle",
    hdrs = ["constant_handle.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//common:result",
        "//common:strong_identifier_type",
        "//common/constant:category",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
    ],
)

cc_library(
    name = "functions",
    hdrs = ["functions.h"],
    srcs = ["functions.cc"],
    deps = [
        "//common/constant:component",
        "@com_google_absl//absl/container:flat_hash_set",
        "@nth_cc//nth/utility:no_destructor",
    ],
)

cc_library(
    name = "identifiers",
    hdrs = ["identifiers.h"],
    srcs = ["identifiers.cc"],
    deps = [
        "@nth_cc//nth/container:flyweight_set",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/utility:no_destructor",
    ],
)

cc_library(
    name = "integers",
    hdrs = ["integers.h"],
    srcs = ["integers.cc"],
    deps = [
        "//common:result",
        "//common/constant:entry",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/container:flyweight_map",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/numeric:integer",
        "@nth_cc//nth/utility:no_destructor",
    ],
)

cc_library(
    name = "parameters",
    hdrs = ["parameters.h"],
    srcs = ["parameters.cc"],
    deps = [
        "//common:identifier",
        "//common/constant:component",
        "@com_google_absl//absl/container:flat_hash_set",
        "@nth_cc//nth/utility:no_destructor",
    ],
)

cc_library(
    name = "strings",
    hdrs = ["strings.h"],
    srcs = ["strings.cc"],
    deps = [
        "//common:result",
        "//common:to_bytes",
        "//common/constant:entry",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/container:flyweight_map",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/utility:no_destructor",
    ],
)
