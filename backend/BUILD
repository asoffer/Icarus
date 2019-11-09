package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "eval",
    deps = [
        ":exec",
        "//ast:expression",
        "//base:debug",
        "//base:untyped_buffer",
        "//compiler:compiler",
        "//core:arch",
        "//ir:compiled_fn",
        "//ir/cmd:jumps",
        "//ir/cmd:return",
        "//type:generic_struct",
        "//type:util",
    ],
    test_deps = None,
)

cc_lib(
    name = "exec",
    deps = [
        "//ast:ast",
        "//base:untyped_buffer",
        "//base:util",
        "//core:arch",
        "//error:log",
        "//ir:addr",
        "//ir:block_def",
        "//ir:compiled_fn",
        "//ir:reg",
        "//ir:reg_or",
        "//ir:read_only_data",
        "//type:generic_struct",
        "//type:util",
    ],
    test_deps = None,
)
