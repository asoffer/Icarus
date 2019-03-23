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
        "//backend:eval-impl",
        "//backend:exec-impl",
        "//core:impl",
        "//frontend:lex-impl",
        "//frontend:parse",
        "//error:log-impl",
        "//ir:impl",
        "//misc:impl",
        "//property:property_map-impl",
        "//property:property_set-impl",
        "//property:property-impl",
        "//type:impl",
    ],
    alwayslink = True,
)
