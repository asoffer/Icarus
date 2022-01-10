package(default_visibility = ["//visibility:public"])

cc_library(
    name = "combine_blocks",
    hdrs = ["combine_blocks.h"],
    srcs = ["combine_blocks.cc"],
    deps = [
        "//base:log",
        "//ir/instruction:core",
        "//ir:compiled_fn",
    ],
)

cc_library(
    name = "opt",
    hdrs = ["opt.h"],
    srcs = ["opt.cc"],
    deps = [
        ":combine_blocks",
        "//ir:compiled_fn",
    ],
)
