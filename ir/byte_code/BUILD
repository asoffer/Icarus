package(default_visibility = ["//visibility:public"])

cc_library(
    name = "byte_code",
    hdrs = ["byte_code.h"],
    deps = [
        "//base:untyped_buffer",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "context",
    hdrs = ["context.h"],
    deps = [
        ":byte_code",
        "//ir/value:native_fn",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_library(
    name = "reader",
    hdrs = ["reader.h"],
    deps = [
        ":byte_code",
        "//base:serialize",
        "//base:untyped_buffer",
        "@com_google_absl//absl/types:span",
    ]
)

cc_library(
    name = "writer",
    hdrs = ["writer.h"],
    deps = [
        ":byte_code",
        "//base:debug",
        "//base:serialize",
        "//base:untyped_buffer",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ]
)
