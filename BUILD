package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target")

cc_binary(
    name = "icarus",
    srcs = ["main.cc"],
    deps = [
        ":impl-compile",
        "//diagnostic:console_renderer-impl-compile",
        "//init:cli-impl",
        "//init:signal-impl",
        "//run:repl-impl-compile",
        "//run:compiler-impl-compile",
        "@com_google_absl//absl/strings",
    ],
)

cc_binary(
    name = "icmatch",
    srcs = ["match.cc"],
    deps = [
        ":impl-match",
        "//base:macros",
        "//diagnostic:console_renderer-impl-compile",
        "//init:cli-impl",
        "//init:signal-impl",
        "//match:binding_id-impl-match",
    ],
)

COMMON_IMPL_DEPS = [
        "//ast:dispatch_table-impl",
        "//ast:overload_set-impl",
        "//ast/scope:scope-impl",
        "//ast/scope:exec-impl",
        "//base:log-impl",
        "//base:untyped_buffer-impl",
        "//backend:eval-impl",
        "//backend:exec-impl",
        "//compiler:compiler-impl",
        "//compiler:constant_binding-impl",
        "//compiler:module-impl",
        "//compiler:extract_jumps-impl",
        "//compiler/dispatch:dispatch-impl",
        "//compiler/dispatch:extract_params-impl",
        "//compiler/dispatch:match-impl",
        "//error:log-impl",
        "//frontend:lex-impl",
        "//frontend:parse-impl",
        "//frontend/source:file-impl",
        "//ir:block_def-impl",
        "//ir:foreign-impl",
        "//ir:impl",
        "//module:module-impl",
        "//module:pending-impl",
        "//opt:combine_blocks-impl",
        "//type:impl",
        "//type:cast-impl",
        "//module:assign_scope-impl",
        "//module:dependent_decls-impl",
        "//module:extract_defining_modules-impl",
        "//ast/methods:dump-impl",
    ]

cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = {
        "compile": COMMON_IMPL_DEPS,
        "match": COMMON_IMPL_DEPS + [
            "//match:match_expr-impl",
            "//match:binding_id-impl",
        ],
    }
)
