package(default_visibility = ["//visibility:public"])

cc_library(
    name = "message",
    hdrs = ["message.h"],
    deps = [
        "//base:meta",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "console_renderer",
    hdrs = ["console_renderer.h"],
    srcs = ["console_renderer.cc"],
    deps = [
        ":message",
        "//base:meta",
        "//base:interval",
        "//frontend:source_indexer",
        "@com_google_absl//absl/strings:str_format",
    ],
)
