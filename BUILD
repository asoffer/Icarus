package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target")

cc_binary(
    name = "icarus",
    srcs = ["main.cc"],
    deps = [
        ":impl-compile",
        "//init:cli-impl",
        "//init:signal-impl",
        "//misc:compile-impl-compile",
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
        "//init:cli-impl",
        "//init:signal-impl",
        "//match:binding_id-impl-match",
    ],
)

COMMON_IMPL_DEPS = [
        "//ast:dispatch_table-impl",
        "//ast:overload_set-impl",
        "//base:log-impl",
        "//base:untyped_buffer-impl",
        "//backend:eval-impl",
        "//backend:exec-impl",
        "//compiler:compiler-impl",
        "//core:pending_module-impl",
        "//core:scope-impl",
        "//frontend:lex-impl",
        "//frontend:parse-impl",
        "//error:log-impl",
        "//ir:block_def-impl",
        "//ir:foreign-impl",
        "//ir:impl",
        "//misc:impl",
        "//opt:combine_blocks-impl",
        "//type:impl",
        "//type:cast-impl",
        "//visitor:visitors-impl",
        "//visitor:type_visitors-impl",
    ]

cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = {
        "compile": COMMON_IMPL_DEPS,
        "match": COMMON_IMPL_DEPS + ["//match:binding_id-impl"],
    }
)
