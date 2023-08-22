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
