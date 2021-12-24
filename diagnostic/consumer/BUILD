package(default_visibility = ["//visibility:public"])

cc_library(
    name = "buffering",
    hdrs = ["buffering.h"],
    deps = [
        ":consumer",
        "//diagnostic:console_renderer",
        "//diagnostic:message",
        "//frontend/source:buffer",
    ],
)

cc_library(
    name = "consumer",
    hdrs = ["consumer.h"],
    deps = [
        "//base:debug",
        "//diagnostic:message",
        "//frontend/source:buffer",
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
        "//frontend/source:buffer",
    ],
)

cc_library(
    name = "tracking",
    hdrs = ["tracking.h"],
    deps = [
        ":consumer",
        "//diagnostic:message",
        "//frontend/source:buffer",
    ],
    testonly = True,
)

cc_library(
    name = "trivial",
    hdrs = ["trivial.h"],
    deps = [
        ":consumer",
        "//diagnostic:message",
        "//frontend/source:buffer",
    ],
)

cc_library(
    name = "aborting",
    hdrs = ["aborting.h"],
    deps = [
        ":consumer",
        "//diagnostic:message",
        "//frontend/source:buffer",
    ],
)
