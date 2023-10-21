package(default_visibility = ["//visibility:public"])

cc_library(
    name = "deserialize",
    hdrs = ["deserialize.h"],
    srcs = ["deserialize.cc"],
    deps = [
        ":type",
        ":type_system_cc_proto",
    ],
)

cc_library(
    name = "type",
    hdrs = ["type.h"],
    srcs = ["type.cc"],
    deps = [
        "//common/language:primitive_types",
        "//common/language:type_kind",
        "@asoffer_jasmin//jasmin:value",
        "@asoffer_nth//nth/container:flyweight_set",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/utility:no_destructor",
    ],
)

proto_library(
    name = "type_system_proto",
    srcs = ["type_system.proto"],
    deps = [],
)

cc_proto_library(
    name = "type_system_cc_proto",
    deps = [":type_system_proto"],
)

cc_test(
    name = "type_test",
    srcs = ["type_test.cc"],
    deps = [
        ":type",
        "@asoffer_nth//nth/test:main",
    ]
)
