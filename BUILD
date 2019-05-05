package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "icarus",
    srcs = ["main.cc"],
    deps = [
        ":impl",
        "//init:cli-impl",
        "//init:signal-impl",
        "//run:repl",
        "//run:compiler",
    ],
)

cc_library(
    name = "impl",
    deps = [
        "//ast:impl",
        "//base:untyped_buffer-impl",
        "//backend:eval-impl",
        "//backend:exec-impl",
        "//core:impl",
        "//frontend:lex-impl",
        "//frontend:parse",
        "//error:log-impl",
        "//ir:impl",
        "//misc:impl",
        "//opt:impl",
        "//property:property_map-impl",
        "//property:property_set-impl",
        "//property:property-impl",
        "//type:impl",
    ],
    alwayslink = True,
)

filegroup(name = "sources", srcs = ["main.cc"])

genrule(
    name = "unity-source",
    srcs = [
        ":sources",
        "//ast:sources",
        "//backend:sources",
        "//base:sources",
        "//core:sources",
        "//error:sources",
        "//frontend:sources",
        "//init:sources",
        "//ir:sources",
        "//misc:sources",
        "//opt:sources",
        "//property:sources",
        "//run:sources",
        "//type:sources",
    ],
    outs = ["unity.cc"],
    cmd = ("cat $(locations :sources)" +
           " $(locations //ast:sources)" +
           " $(locations //backend:sources)" +
           " $(locations //base:sources)" +
           " $(locations //core:sources)" +
           " $(locations //error:sources)" +
           " $(locations //frontend:sources)" +
           " $(locations //init:sources)" +
           " $(locations //ir:sources)" +
           " $(locations //misc:sources)" +
           " $(locations //opt:sources)" +
           " $(locations //property:sources)" +
           " $(locations //run:sources)" +
           " $(locations //type:sources)" +
           " > $@"),
)

cc_binary(
    name = "icarus-unity",
    srcs = ["unity.cc"],
    deps = [
        "//ast",
        "//backend:eval",
        "//base:guarded",
        "//base:permutation",
        "//frontend:lex",
        "//frontend:numbers",
        "//frontend:source",
        "//frontend:tagged_node",
        "//frontend:token",
        "//init:cli",
        "//init:signal",
        "//ir:components",
        "//ir:phi",
        "//ir:str",
        "//misc:import_graph",
        "//opt:combine_blocks",
        "//type:cast",
    ],
)
