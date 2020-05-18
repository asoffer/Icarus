package(default_visibility = ["//visibility:public"])

cc_library(
    name = "errors",
    hdrs = ["errors.h"],
    deps = [
        ":message",
        "//frontend/lex:numbers",
        "//frontend/source",
        "//frontend/source:range",
        "//interpretter:evaluation_failure",
        "//type:qual_type",
        "//type:tuple",
    ],
)

cc_library(
    name = "message",
    hdrs = ["message.h"],
    deps = [
        "//frontend/source",
        "//frontend/source:range",
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
        "@com_google_absl//absl/strings:str_format",
    ],
)
