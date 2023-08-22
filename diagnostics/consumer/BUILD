package(default_visibility = ["//visibility:public"])

cc_library(
    name = "consumer",
    hdrs = ["consumer.h"],
    deps = [
        "//diagnostics:message",
    ],
)

cc_library(
    name = "null",
    hdrs = ["null.h"],
    deps = [
        ":consumer",
    ],
)

cc_library(
    name = "streaming",
    hdrs = ["streaming.h"],
    srcs = ["streaming.cc"],
    deps = [
        ":consumer",
        "//diagnostics:message",
        "@com_google_absl//absl/strings:str_format",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/strings:text",
    ],
)
