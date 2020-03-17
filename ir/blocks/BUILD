package(default_visibility = ["//visibility:public"])

cc_library(
    name = "basic",
    hdrs = ["basic.h"],
    srcs = ["basic.cc"],
    deps = [
        "//ir:byte_code_writer",
        "//ir/instruction:base",
        "//ir/instruction:op_codes",
        "//ir/instruction:jump",
        "//base:ptr_span",
        "//base:untyped_buffer",
        "//type:type",
    ],
)
