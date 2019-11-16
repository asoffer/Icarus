package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "icarus",
    srcs = ["main.cc"],
    deps = [
        "//base:log",
        "//init:cli",
        "//run:repl",
        "//run:compiler",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
        "@com_google_absl//absl/strings",
    ],
)
