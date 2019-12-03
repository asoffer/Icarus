package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "eval",
    deps = [
        ":exec",
        "//base:debug",
        "//base:untyped_buffer",
        "//core:arch",
        "//ir:compiled_fn",
        "//ir:results",
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
        "//ir:basic_block",
        "//ir:block_def",
        "//ir/cmd:cmd_xmacro",
        "//ir:compiled_fn",
        "//ir:reg",
        "//ir:reg_or",
        "//ir:read_only_data",
        "//ir:scope_def",
        "//type:generic_struct",
        "//type:util",
    ],
    test_deps = None,
)
