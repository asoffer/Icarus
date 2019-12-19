package(default_visibility = ["//visibility:public"])

cc_library(
    name = "consumer",
    hdrs = ["consumer.h"],
    deps = [
        "//diagnostic:message"
    ],
)

cc_library(
    name = "streaming",
    hdrs = ["streaming.h"],
    deps = [
        ":consumer",
        "//diagnostic:console_renderer",
        "//diagnostic:message"
    ],
)
