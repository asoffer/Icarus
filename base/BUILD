package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "check",
    hdrs = ["check.h"],
    deps = [":string"],
)

cc_lib(
    name = "debug",
    hdrs = ["debug.h"],
    deps = [
        ":check",
        ":log",
    ]
)

cc_lib(
    name = "expected",
    hdrs = ["expected.h"],
)

cc_lib(
    name = "guarded",
    hdrs = ["guarded.h"],
)

cc_lib(
    name = "hash",
    hdrs = ["hash.h"],
)

cc_lib(
    name = "interval",
    hdrs = ["interval.h"],
)

cc_lib(
    name = "lazy",
    hdrs = ["lazy.h"],
)

cc_lib(
    name = "log",
    hdrs = ["log.h"],
    deps = [":string"],
)

cc_lib(
    name = "owned_ptr",
    hdrs = ["owned_ptr.h"],
)

cc_lib(
    name = "string",
    hdrs = ["string.h"],
    visibility = ["//visibility:private"],
)

cc_lib(
    name = "strong_types",
    hdrs = ["strong_types.h"],
    deps = [":debug"],
)

cc_lib(
    name = "test",
    hdrs = ["test.h"],
    deps = [":check"],
)

cc_lib(
    name = "types",
    hdrs = ["types.h"],
)

cc_lib(
    name = "untyped_buffer",
    hdrs = ["untyped_buffer.h"],
    srcs = ["untyped_buffer.cc"],
    deps = [":debug"],
)

cc_lib(
    name = "util",
    hdrs = ["util.h"],
    deps = [":debug"],
)
