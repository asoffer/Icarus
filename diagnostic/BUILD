package(default_visibility = ["//visibility:public"])

cc_library(
    name = "message",
    hdrs = ["message.h"],
    deps = [
        "@com_google_absl//absl/strings:str_format",
        "@asoffer_nth//nth/meta:type",
    ],
)

cc_library(
    name = "console_renderer",
    hdrs = ["console_renderer.h"],
    srcs = ["console_renderer.cc"],
    deps = [
        ":message",
        "//frontend:source_indexer",
        "@asoffer_nth//nth/meta:type",
        "@asoffer_nth//nth/io:file",
        "@com_google_absl//absl/strings:str_format",
    ],
)
