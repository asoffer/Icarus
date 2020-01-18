package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_library(
    name = "clone",
    hdrs = ["clone.h"],
    deps = []
)

cc_test(
    name = "clone_test",
    srcs = ["clone_test.cc"],
    deps = [
        ":clone",
        "//test",
    ]
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
 
cc_library(
    name = "guarded", 
    hdrs = ["guarded.h"],
    deps = [],
)

cc_test(
    name = "guarded_test",
    srcs = ["guarded_test.cc"],
    deps = [
        ":guarded",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_lib(
    name = "interval",
    deps = [":debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "lazy", 
    deps = [":debug"],
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
        "@com_google_absl//absl/strings",
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

cc_library(
    name = "move_func",
    hdrs = ["move_func.h"],
)

cc_test(
    name = "move_func_test",
    srcs = ["move_func_test.cc"],
    deps = [
        ":move_func",
        "//test",
    ],
)

cc_lib(
    name = "permutation", 
    deps = ["@com_google_absl//absl/random"],
    header_only = True,
    test_deps = None,
)

cc_library(
    name = "raw_iterator",
    hdrs = ["raw_iterator.h"],
    deps = [":unaligned_ref"],
)

cc_test(
    name = "raw_iterator_test",
    srcs = ["raw_iterator_test.cc"],
    deps = [
        ":raw_iterator",
        "@com_google_googletest//:gtest_main",
    ]
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

cc_library(
    name = "untyped_buffer",
    hdrs = ["untyped_buffer.h"],
    srcs = ["untyped_buffer.cc"],
    deps = [
        ":debug",
        ":raw_iterator",
        ":unaligned_ref",
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

cc_library(
    name = "untyped_buffer_view",
    hdrs = ["untyped_buffer_view.h"],
    deps = [
        ":debug",
        ":raw_iterator",
        ":untyped_buffer",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "untyped_buffer_view_test",
    srcs = ["untyped_buffer_view_test.cc"],
    deps = [
        ":untyped_buffer",
        ":untyped_buffer_view",
        "@com_google_googletest//:gtest_main",
    ],
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
