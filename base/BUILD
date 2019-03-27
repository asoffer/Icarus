package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "bag",
    hdrs = ["bag.h"],
)

cc_lib(
    name = "debug",
    hdrs = ["debug.h"],
    deps = [
        ":matchers",
        ":log",
    ]
)

cc_lib(
    name = "expected",
    hdrs = ["expected.h"],
    deps = [":stringify"],
)

cc_lib(
    name = "graph",
    hdrs = ["graph.h"],
    deps = [
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
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
    deps = [":stringify"],
)

cc_lib(
    name = "macros",
    hdrs = ["macros.h"],
)

cc_lib(
    name = "matchers",
    hdrs = ["matchers.h"],
    deps = [":tuple"],
)

cc_lib(
    name = "owned_ptr",
    hdrs = ["owned_ptr.h"],
)

cc_lib(
    name = "stringify",
    hdrs = ["stringify.h"],
    deps = [":tuple"],
)

cc_test(
    name = "stringify_test",
    srcs = ["stringify_test.cc"],
    deps = [
        ":stringify",
        "//test",
    ],
)

cc_lib(
    name = "strong_types",
    hdrs = ["strong_types.h"],
    deps = [":debug"],
)

cc_lib(
    name = "tuple",
    hdrs = ["tuple.h"],
)

cc_test(
    name = "tuple_test",
    srcs = ["tuple_test.cc"],
    deps = [
        ":tuple",
        "//test",
    ],
)

cc_lib(
    name = "untyped_buffer",
    hdrs = ["untyped_buffer.h"],
    srcs = ["untyped_buffer.cc"],
    deps = [
        ":debug",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "untyped_buffer_test",
    srcs = ["untyped_buffer_test.cc"],
    deps = [
        ":untyped_buffer",
        "//test",
    ],
)

cc_lib(
    name = "untyped_map",
    hdrs = ["untyped_map.h"],
    deps = [
        ":untyped_buffer",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "untyped_map_test",
    srcs = ["untyped_map_test.cc"],
    deps = [
        ":untyped_map",
        "//test",
    ],
)

cc_lib(
    name = "util",
    hdrs = ["util.h"],
    deps = [
        ":debug",
        ":macros",
    ],
)
