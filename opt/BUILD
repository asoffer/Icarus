package(default_visibility = ["//visibility:public"])

cc_library(
    name = "combine_blocks",
    hdrs = ["combine_blocks.h"],
    srcs = ["combine_blocks.cc"],
    deps = [
        "//base:log",
        "//ir/instruction:core",
        "//ir:subroutine",
    ],
)

cc_library(
    name = "opt",
    hdrs = ["opt.h"],
    srcs = ["opt.cc"],
    deps = [
        ":combine_blocks",
        "//ir:subroutine",
    ],
)
