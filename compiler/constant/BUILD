package(default_visibility = ["//visibility:public"])

cc_library(
    name = "binding",
    hdrs = ["binding.h"],
    srcs = ["binding.cc"],
    deps = [
        "//ast:ast",
        "//base:untyped_buffer",
        "//base:untyped_buffer_view",
        "//core:arch",
        "//core:bytes",
        "//ir:results",
        "//type:type_fwd",
        "//type:type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)
