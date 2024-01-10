package(default_visibility = ["//visibility:public"])

cc_library(
    name = "alignment",
    hdrs = ["alignment.h"],
)

cc_test(
    name = "alignment_test",
    srcs = ["alignment_test.cc"],
    deps = [
        ":alignment",
        "@asoffer_nth//nth/test:main",
    ]
)

cc_library(
    name = "basic",
    hdrs = ["basic.h"],
    deps = [
        ":type_contour",
        "//common/language:type_kind",
        "@asoffer_jasmin//jasmin/core:value",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "byte_width",
    hdrs = ["byte_width.h"],
    srcs = ["byte_width.cc"],
    deps = [
        ":alignment"
    ],
)

cc_test(
    name = "byte_width_test",
    srcs = ["byte_width_test.cc"],
    deps = [
        ":byte_width",
        "@asoffer_nth//nth/test:main",
    ]
)

cc_library(
    name = "cast",
    hdrs = ["cast.h"],
    srcs = ["cast.cc"],
    deps = [
        ":type",
        "//common:any_value",
    ],
)

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
    name = "dependent",
    hdrs = ["dependent.h"],
    srcs = ["dependent.cc"],
    deps = [
        ":basic",
        ":function",
        ":refinement",
        "//common:any_value",
        "@asoffer_jasmin//jasmin/core:function",
        "@asoffer_jasmin//jasmin/core:value",
        "@asoffer_nth//nth/container:flyweight_set",
        "@asoffer_nth//nth/container:interval",
        "@asoffer_nth//nth/debug",
    ],
)

cc_test(
    name = "dependent_test",
    srcs = ["dependent_test.cc"],
    deps = [
        ":dependent",
        ":parameters",
        ":primitive",
        ":type",
        "//common:any_value",
        "@asoffer_jasmin//jasmin/core:value",
        "@asoffer_jasmin//jasmin/instructions:arithmetic",
        "@asoffer_jasmin//jasmin/instructions:common",
        "@asoffer_nth//nth/test:main",
    ]
)

cc_library(
    name = "function",
    hdrs = ["function.h"],
    deps = [
        ":basic",
        ":parameters",
        ":type_system_cc_proto",
        "//common/language:type_kind",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "opaque",
    hdrs = ["opaque.h"],
    deps = [
        ":basic",
        "//common/language:type_kind",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "parameters",
    hdrs = ["parameters.h"],
    deps = [
        ":basic",
        "//common/language:type_kind",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "pattern",
    hdrs = ["pattern.h"],
    deps = [
        ":basic",
        "//common/language:type_kind",
    ],
)

cc_library(
    name = "pointer",
    hdrs = ["pointer.h"],
    deps = [
        ":basic",
        "//common/language:type_kind",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "primitive",
    hdrs = ["primitive.h"],
    deps = [
        ":basic",
        "//common/language:primitive_types",
        "//common/language:type_kind",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "refinement",
    hdrs = ["refinement.h"],
    deps = [
        ":basic",
        ":primitive",
        "//common/language:type_kind",
        "//common:any_value",
        "//common:pattern",
        "@asoffer_jasmin//jasmin/core:function",
        "@asoffer_nth//nth/debug",
    ],
)

cc_library(
    name = "serialize",
    hdrs = ["serialize.h"],
    srcs = ["serialize.cc"],
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
        ":alignment",
        ":basic",
        ":byte_width",
        ":dependent",
        ":function",
        ":opaque",
        ":parameters",
        ":pattern",
        ":pointer",
        ":primitive",
        ":refinement",
        ":type_contour",
        ":type_system_cc_proto",
        "//common/language:type_kind",
        "//common:pattern",
        "@asoffer_nth//nth/container:flyweight_set",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/utility:no_destructor",
    ],
)

cc_library(
    name = "type_contour",
    hdrs = ["type_contour.h"],
    deps = [
        ":alignment",
        ":byte_width",
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
