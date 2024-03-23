package(default_visibility = ["//visibility:public"])

cc_library(
    name = "category",
    hdrs = ["category.h"],
    deps = [
        "//common/language:type_kind",
    ],
)

cc_library(
    name = "component",
    hdrs = ["component.h"],
    deps = [
        ":category",
        "//common:result",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
        "@nth_cc//nth/utility:bytes",
    ],
)

cc_library(
    name = "entry",
    hdrs = ["entry.h"],
    deps = [],
)

cc_library(
    name = "manifest",
    hdrs = ["manifest.h"],
    srcs = ["manifest.cc"],
    deps = [
        ":category",
        ":component",
        "//common:result",
        "//common/internal:functions",
        "//common/internal:identifiers",
        "//common/internal:integers",
        "//common/internal:parameters",
        "//common/internal:strings",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
    ],
)
