package(default_visibility = ["//visibility:public"])

cc_library(
    name = "errno",
    hdrs = ["errno.h"],
    deps = [],
)

cc_test(
    name = "errno_test",
    srcs = ["errno_test.cc"],
    deps = [
        ":errno",
        "@asoffer_nth//nth/test:main",
    ]
)

cc_library(
    name = "string",
    hdrs = ["string.h"],
    deps = [
        "@asoffer_nth//nth/io:string_printer",
        "@asoffer_nth//nth/strings:interpolate",
        "@asoffer_nth//nth/strings/format:universal",
    ],
)

cc_library(
    name = "resources",
    hdrs = ["resources.h"],
    deps = [],
)
