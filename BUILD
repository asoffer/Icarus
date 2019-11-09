package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target")

COMMON_IMPL_DEPS = [
    "//ast:overload_set",
    "//ast/scope:scope",
    "//ast/scope:exec",
    "//base:log",
    "//base:untyped_buffer",
    "//compiler:extract_jumps",
    "//error:log",
    "//frontend:lex",
    "//frontend:parse",
    "//frontend/source:file",
    "//ir:block_def",
    "//ir:foreign",
    "//module:module",
    "//module:pending",
    "//opt:combine_blocks",
    "//type:cast",
    "//module:assign_scope",
    "//module:dependent_decls",
    "//ast/methods:dump",
]

cc_binary(
    name = "icarus",
    srcs = ["main.cc"],
    deps = COMMON_IMPL_DEPS + [
        "//diagnostic:console_renderer",
        "//init:cli",
        "//init:signal",
        "//run:repl-impl",
        "//run:compiler-impl",
        "@com_google_absl//absl/strings",
    ],
)

cc_binary(
    name = "icmatch",
    srcs = ["match.cc"],
    defines = ["ICARUS_MATCHER"],
    deps = COMMON_IMPL_DEPS + [
        "//base:macros",
        "//diagnostic:console_renderer",
        "//init:cli",
        "//init:signal",
        "//match:binding_id",
        "//match:match_expr",
    ],
)
