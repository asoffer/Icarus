package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_group_target", "cc_lib_target")

cc_binary(
    name = "icarus",
    srcs = ["main.cc"],
    deps = [
        ":impl-compile",
        "//init:cli-impl-compile",
        "//init:signal-impl-compile",
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
        "//init:cli-impl-match",
        "//init:signal-impl-match",
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
        "//core:impl",
        "//frontend:lex-impl",
        "//frontend:parse-impl",
        "//error:log-impl",
        "//ir:impl",
        "//misc:impl",
        "//opt:impl",
        "//type:impl",
        "//type:cast-impl",
        "//visitor:visitors-impl",
        "//visitor:type_visitors-impl",
        "//visitor:special_function-impl",
    ]
cc_group_target(
    name = "impl",
    cfgs = ["match", "compile"],
    deps = {
        "compile": COMMON_IMPL_DEPS,
        "match": COMMON_IMPL_DEPS + ["//match:binding_id-impl"],
    }
)

filegroup(name = "sources", srcs = ["main.cc"])

# genrule(
#     name = "unity-source",
#     srcs = [
#         ":sources",
#         "//ast:sources",
#         "//visitor:sources",
#         "//backend:sources",
#         "//base:sources",
#         "//core:sources",
#         "//error:sources",
#         "//frontend:sources",
#         "//init:sources",
#         "//ir:sources",
#         "//misc:sources",
#         "//opt:sources",
#         "//run:sources",
#         "//type:sources",
#     ],
#     outs = ["unity.cc"],
#     cmd = ("cat $(locations :sources)" +
#            " $(locations //ast:sources)" +
#            " $(locations //visitor:sources)" +
#            " $(locations //backend:sources)" +
#            " $(locations //base:sources)" +
#            " $(locations //core:sources)" +
#            " $(locations //error:sources)" +
#            " $(locations //frontend:sources)" +
#            " $(locations //init:sources)" +
#            " $(locations //ir:sources)" +
#            " $(locations //misc:sources)" +
#            " $(locations //opt:sources)" +
#            " $(locations //run:sources)" +
#            " $(locations //type:sources)" +
#            " > $@"),
# )
# 
# cc_binary(
#     name = "icarus-unity",
#     srcs = ["unity.cc"],
#     deps = [
#         "//ast",
#         "//backend:eval",
#         "//base:guarded",
#         "//base:permutation",
#         "//frontend:lex",
#         "//frontend:numbers",
#         "//frontend:source",
#         "//frontend:tagged_node",
#         "//frontend:token",
#         "//init:cli",
#         "//init:signal",
#         "//ir:components",
#         "//ir:phi",
#         "//ir:str",
#         "//opt:combine_blocks",
#         "//type:cast",
#         "//visitor:special_function",
#         "@com_google_absl//absl/container:node_hash_set",
#     ],
# )
