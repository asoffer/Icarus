package(default_visibility = ["//common:__subpackages__"])

cc_library(
    name = "component",
    hdrs = ["component.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//common:result",
        "//common/language:type_kind",
        "@nth_cc//nth/utility:bytes",
    ],
)

cc_library(
    name = "constant_handle",
    hdrs = ["constant_handle.h"],
    deps = [
        "//common:result",
        "//common:strong_identifier_type",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
    ],
)

