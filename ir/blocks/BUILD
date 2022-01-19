package(default_visibility = ["//visibility:public"])

cc_library(
    name = "offset_cache",
    hdrs = ["offset_cache.h"],
    deps = [
        "//ir/value:addr",
        "//ir/value:reg_or",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "offset_cache_test",
    srcs = ["offset_cache_test.cc"],
    deps = [
        ":offset_cache",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "load_store_cache",
    hdrs = ["load_store_cache.h"],
    deps = [
        "//base:meta",
        "//base:unaligned_ref",
        "//base:untyped_buffer",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "load_store_cache_test",
    srcs = ["load_store_cache_test.cc"],
    deps = [
        ":load_store_cache",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "register_allocator",
    hdrs = ["register_allocator.h"],
    deps = [
        "//ir/value:addr",
        "//ir/value:reg",
        "//type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)

cc_test(
    name = "register_allocator_test",
    srcs = ["register_allocator_test.cc"],
    deps = [
        ":register_allocator",
        "//type:fake",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ],
)
