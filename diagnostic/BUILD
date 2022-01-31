package(default_visibility = ["//visibility:public"])

cc_library(
    name = "message",
    hdrs = ["message.h"],
    srcs = ["message.cc"],
    deps = [
        "//base:meta",
        "//frontend/source",
        "//frontend/source:buffer",
        "//type",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "console_renderer",
    hdrs = ["console_renderer.h"],
    srcs = ["console_renderer.cc"],
    deps = [
        ":message",
        "//base:macros",
        "//base:meta",
        "//base:interval",
        "//frontend/source:indexer",
        "@com_google_absl//absl/strings:str_format",
    ],
)
