package(default_visibility = ["//visibility:public"])

cc_library(
    name = "context",
    hdrs = ["context.h"],
    srcs = ["context.cc"],
    deps = [
        ":instruction_set",
        ":type_system",
        "//ast",
        "//base:debug",
        "//module:symbol",
    ]
)

cc_library(
    name = "instruction_set",
    hdrs = [
        "instruction_set.h",
    ],
    srcs = [
        "instruction_set.cc",
    ],
    deps = [
        ":type_system",
        "//core/type_system",
        "//data_types:integer",
        "//data_types:fn",
        "//serialization:foreign_symbol_map",
        "//serialization:module_index",
        "//serialization:read_only_data",
        "@asoffer_nth//nth/numeric:integer",
        "@asoffer_jasmin//jasmin:debug",
        "@asoffer_jasmin//jasmin:function",
        "@asoffer_jasmin//jasmin/instructions:arithmetic",
        "@asoffer_jasmin//jasmin/instructions:bool",
        "@asoffer_jasmin//jasmin/instructions:compare",
        "@asoffer_jasmin//jasmin/instructions:core",
        "@asoffer_jasmin//jasmin/instructions:stack",
        "@asoffer_jasmin//jasmin:serialization",
        "@asoffer_nth//nth/container:flyweight_map",
        "@asoffer_nth//nth/container:flyweight_set",
    ],
)

cc_library(
    name = "type_system",
    hdrs = ["type_system.h"],
    srcs = ["type_system.cc"],
    deps = [
        "//base:extend",
        "//base/extend:absl_hash",
        "//core:alignment",
        "//core:arch",
        "//core:bytes",
        "//core:type_contour",
        "//core/type_system",
        "//core/type_system:finite_set",
        "//core/type_system:function",
        "//core/type_system:parameter",
        "//core/type_system:pointer",
        "//core/type_system:sized_integer",
        "//data_types:integer",
        "//data_types:addr",
        "//data_types:char",
    ]
)

cc_test(
    name = "type_system_test",
    srcs = ["type_system_test.cc"],
    deps = [
        ":type_system",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "task",
    hdrs = ["task.h"],
    deps = [
        "//ast:node",
        "//base:extend",
        "//base/extend:absl_hash",
        "//base:debug",
        "@asoffer_nth//nth/meta:type",
        "@com_google_absl//absl/functional:any_invocable",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
)

cc_test(
    name = "task_test",
    srcs = ["task_test.cc"],
    deps = [
        ":task",
        "@com_google_googletest//:gtest_main",
    ],
)
