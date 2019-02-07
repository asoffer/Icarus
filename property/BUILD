package(default_visibility = ["//visibility:public"])

cc_library(
    name = "property",
    hdrs = ["property.h"],
    srcs = ["property.cc"],
    deps = [
        "//base:owned_ptr",
        "//base:util",
        "//ir:cmd",
    ],
)

cc_library(
    name = "property_map",
    hdrs = ["property_map.h"],
    deps = [
        ":property",
        ":property_set",
        "//base/container:unordered_map",
        "//base:hash",
        "//base:owned_ptr",
        "//base:util",
        "//ir:cmd",
    ],
)

cc_library(
    name = "property_map-impl",
    srcs = ["property_map.cc"],
    deps = [
        ":property",
        "//misc:architecture",
        "//ir:arguments",
        "//ir:func",
        "//type:function",
        "//type:generic_struct",
    ],
)

cc_library(
    name = "property_set",
    hdrs = ["property_set.h"],
    srcs = ["property_set.cc"],
    deps = [
        ":property",
        "//base/container:bag",
        "//base:owned_ptr",
        "//base:util",
    ],
)
