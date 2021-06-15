package(default_visibility = ["//visibility:public"])

cc_library(
    name = "buffering",
    hdrs = ["buffering.h"],
    deps = [
        ":consumer",
        "//diagnostic:message",
    ],
)

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
        "//base:debug",
        "//diagnostic:console_renderer",
        "//diagnostic:message",
    ],
)

cc_library(
    name = "tracking",
    hdrs = ["tracking.h"],
    deps = [
        ":consumer",
        "//diagnostic:message"
    ],
    testonly = True,
)

cc_library(
    name = "trivial",
    hdrs = ["trivial.h"],
    deps = [
        ":consumer",
        "//diagnostic:message"
    ],
)

cc_library(
    name = "aborting",
    hdrs = ["aborting.h"],
    deps = [
        ":consumer",
        "//diagnostic:message"
    ],
)
