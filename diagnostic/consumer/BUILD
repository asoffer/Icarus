package(default_visibility = ["//visibility:public"])

cc_library(
    name = "consumer",
    hdrs = ["consumer.h"],
    deps = [
        "//diagnostic"
    ],
)

cc_library(
    name = "streaming",
    hdrs = ["streaming.h"],
    deps = [
        ":consumer",
        "//diagnostic",
        "//diagnostic:console_renderer",
    ],
)
