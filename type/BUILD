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
        "@nth_cc//nth/test:main",
    ]
)

cc_library(
    name = "basic",
    hdrs = ["basic.h"],
    deps = [
        ":type_contour",
        "//common/language:type_kind",
        "@jasmin//jasmin/core:value",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/reader",
        "@nth_cc//nth/io/serialize",
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
        "@nth_cc//nth/test:main",
    ]
)

cc_library(
    name = "cast",
    hdrs = ["cast.h"],
    srcs = ["cast.cc"],
    deps = [
        ":type",
        "//common:any_value",
        "//common:integer",
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
        "//common:result",
        "@jasmin//jasmin/core:function",
        "@jasmin//jasmin/core:value",
        "@nth_cc//nth/container:flyweight_set",
        "@nth_cc//nth/container:interval",
        "@nth_cc//nth/debug",
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
        "@jasmin//jasmin/core:value",
        "@jasmin//jasmin/instructions:arithmetic",
        "@jasmin//jasmin/instructions:common",
        "@nth_cc//nth/test:main",
    ]
)

cc_library(
    name = "function",
    hdrs = ["function.h"],
    deps = [
        ":basic",
        ":parameters",
        "//common/language:type_kind",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
    ],
)

cc_library(
    name = "opaque",
    hdrs = ["opaque.h"],
    deps = [
        ":basic",
        "//common/language:type_kind",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "parameters",
    hdrs = ["parameters.h"],
    deps = [
        ":basic",
        "//common:constants",
        "//common:identifier",
        "//common/language:type_kind",
        "@nth_cc//nth/debug",
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
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "primitive",
    hdrs = ["primitive.h"],
    deps = [
        ":basic",
        "//common:constants",
        "//common/language:primitive_types",
        "//common/language:type_kind",
        "@nth_cc//nth/debug",
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
        "@jasmin//jasmin/core:function",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "type",
    srcs = ["type.cc"],
    deps = [
        ":dependent",
        ":type_contour",
        "//common:constants",
        "@nth_cc//nth/container:flyweight_set",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/utility:no_destructor",
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

cc_test(
    name = "type_test",
    srcs = ["type_test.cc"],
    deps = [
        ":type",
        "@nth_cc//nth/test:main",
    ]
)
