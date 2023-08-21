package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "compile",
    srcs = ["compile.cc"],
    deps = [
        "//ir",
        "//lexer",
        "//parser",
        "@asoffer_nth//nth/commandline:main",
        "@asoffer_nth//nth/debug/log",
        "@asoffer_nth//nth/debug/log:stderr_log_sink",
        "@asoffer_nth//nth/io:file",
        "@asoffer_nth//nth/io:file_path",
        "@asoffer_nth//nth/process:exit_code",
    ],
)
