package(default_visibility = ["//visibility:public"])

cc_library(
    name = "any_invocable",
    hdrs = ["any_invocable.h"],
)

cc_test(
    name = "any_invocable_test",
    srcs = ["any_invocable_test.cc"],
    deps = [
        ":any_invocable",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "cast",
    hdrs = ["cast.h"],
    deps = [":debug"],
)

cc_library(
    name = "debug",
    hdrs = ["debug.h"],
    deps = [":matchers", ":log"],
)

cc_library(
    name = "defer",
    hdrs = ["defer.h"],
    deps = [],
)

cc_test(
    name = "defer_test",
    srcs = ["defer_test.cc"],
    deps = [
        ":defer",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "expected",
    hdrs = ["expected.h"],
    deps = [":stringify"],
)

cc_library(
    name = "extend",
    hdrs = ["extend.h"],
    deps = [
        ":meta",
    ],
)

cc_test(
    name = "extend_test",
    srcs = ["extend_test.cc"],
    deps = [
        ":extend",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "flyweight_map",
    hdrs =["flyweight_map.h"],
    deps = [
        ":debug",
        "@com_google_absl//absl/container:node_hash_map",
    ],
)

cc_library(
    name = "global",
    hdrs = ["global.h"],
    deps = [
        ":guarded",
        ":no_destructor",
    ],
)

cc_library(
    name = "graph",
    hdrs = ["graph.h"],
    deps = [
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)
 
cc_library(
    name = "guarded", 
    hdrs = ["guarded.h"],
    deps = [
        "@com_google_absl//absl/synchronization",
    ],
)

cc_test(
    name = "guarded_test",
    srcs = ["guarded_test.cc"],
    deps = [
        ":guarded",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "interval",
    hdrs = ["interval.h"],
    deps = [":debug"],
)

cc_library(
    name = "lazy", 
    hdrs = ["lazy.h"],
    deps = [":debug"],
)

cc_library(
    name = "log",
    hdrs = ["log.h"],
    srcs = ["log.cc"],
    deps = [
        ":global",
        ":stringify",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/synchronization",
    ],
)

cc_library(
    name = "macros",
    hdrs = ["macros.h"],
    deps = [],
)

cc_library(
    name = "matchers",
    hdrs = ["matchers.h"],
)

cc_library(
    name = "meta",
    hdrs = ["meta.h"],
    deps = [],
)

cc_test(
    name = "meta_test",
    srcs = ["meta_test.cc"],
    deps = [
        ":meta",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "move_func",
    hdrs = ["move_func.h"],
    deps = [":any_invocable"],
)

cc_test(
    name = "move_func_test",
    srcs = ["move_func_test.cc"],
    deps = [
        ":move_func",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "no_destructor",
    hdrs = ["no_destructor.h"],
)

cc_test(
    name = "no_destructor_test",
    srcs = ["no_destructor_test.cc"],
    deps = [
        ":no_destructor",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "permutation", 
    hdrs = ["permutation.h"],
    deps = ["@com_google_absl//absl/random"],
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

cc_library(
    name = "scope",
    hdrs = ["scope.h"],
    deps = [":macros"],
)

cc_test(
    name = "scope_test",
    srcs = ["scope_test.cc"],
    deps = [
        ":scope",
        "@com_google_googletest//:gtest_main",
    ]
)

cc_library(
    name = "stringify",
    hdrs = ["stringify.h"],
    deps = [":unaligned_ref"],
)

cc_test(
    name = "stringify_test",
    srcs = ["stringify_test.cc"],
    deps = [
        ":stringify",
        "@com_google_googletest//:gtest_main",
    ]
)

cc_library(
    name = "strong_types",
    hdrs = ["strong_types.h"],
    deps = [
        ":debug",
        ":meta",
        ":stringify",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "tag",
    hdrs = ["tag.h"],
    deps = [],
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
        "@com_google_googletest//:gtest_main",
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
        "@com_google_googletest//:gtest_main",
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

cc_library(
    name = "ptr_span",
    hdrs = ["ptr_span.h"],
    deps = [],
)
