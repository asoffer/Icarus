package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "compile",
    srcs = ["compile.cc"],
    deps = [
        "//ir",
        "//ir:builtin_module",
        "//ir:emit",
        "//ir:serialize",
        "//ir:module_cc_proto",
        "//common:errno",
        "//common:string",
        "//diagnostics:message",
        "//diagnostics/consumer:streaming",
        "//lexer",
        "//parser",
        "@asoffer_nth//nth/commandline:main",
        "@asoffer_nth//nth/debug/log",
        "@asoffer_nth//nth/io:file",
        "@asoffer_nth//nth/io:file_path",
        "@asoffer_nth//nth/process:exit_code",
        "@com_google_absl//absl/debugging:failure_signal_handler",
        "@com_google_absl//absl/debugging:symbolize",
    ],
)

cc_binary(
    name = "run_bytecode",
    srcs = ["run_bytecode.cc"],
    deps = [
        "//ir:deserialize",
        "//ir:module",
        "//ir:module_cc_proto",
        "//common:errno",
        "//common:string",
        "//diagnostics:message",
        "//diagnostics/consumer:streaming",
        "@asoffer_jasmin//jasmin:execute",
        "@asoffer_nth//nth/commandline:main",
        "@asoffer_nth//nth/debug/log",
        "@asoffer_nth//nth/io:file",
        "@asoffer_nth//nth/io:file_path",
        "@asoffer_nth//nth/process:exit_code",
    ],
)
