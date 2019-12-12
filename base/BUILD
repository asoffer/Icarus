package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "bag",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "debug",
    deps = [":matchers", ":log"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "expected",
    deps = [":stringify"],
    header_only = True,
    test_deps = None,
)


cc_lib(
    name = "cast",
    deps = [":debug"],
    header_only = True,
    test_deps = None,
)


cc_lib(
    name = "graph",
    deps = [
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    header_only = True,
    test_deps = None,
)
 
cc_lib(
    name = "guarded", 
    deps = [],
    header_only = True,
    test_deps = None,
)


cc_lib(
    name = "interval",
    deps = [":debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "lazy", 
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "lazy_convert",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "log",
    deps = [
        ":guarded",
        ":stringify",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    test_deps = None,
)

cc_lib(
    name = "macros",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "matchers",
    deps = [":tuple"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "meta",
    deps = [],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "move_func",
    deps = [],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "permutation", 
    deps = ["@com_google_absl//absl/random"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "scope",
    deps = [":macros"],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "stringify",
    deps = [
        ":tuple",
        ":unaligned_ref",
    ],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "strong_types",
    deps = [
        ":debug",
        ":meta",
        ":stringify",
        "@com_google_absl//absl/strings",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "tag",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "tuple",
    deps = [],
    header_only = True,
    test_deps = [],
)

cc_library(
    name = "unaligned_ref",
    hdrs = ["unaligned_ref.h"],
    deps = [],
)

cc_test(
    name = "unaligned_ref_test",
    srcs = ["unaligned_ref_test.cc"],
    deps = [
        ":unaligned_ref",
        "//test",
    ],
)

cc_lib(
    name = "untyped_buffer",
    deps = [
        ":debug",
        ":unaligned_ref",
        "@com_google_absl//absl/strings",
    ],
    test_deps = [],
)

cc_lib(
    name = "util",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "ptr_span",
    deps = [],
    header_only = True,
    test_deps = None,
)
