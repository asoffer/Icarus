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
        ":primitive",
        ":refinement",
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
    srcs = ["function.cc"],
    deps = [
        ":type",
        ":parameters",
        "//common/constant:manifest",
        "//common/language:type_kind",
        "@com_google_absl//absl/hash",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/debug",
        "@nth_cc//nth/io/deserialize",
        "@nth_cc//nth/io/serialize",
    ],
)

cc_library(
    name = "opaque",
    hdrs = ["opaque.h"],
    srcs = ["opaque.cc"],
    deps = [
        ":type",
        "//common/language:type_kind",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "parameters",
    hdrs = ["parameters.h"],
    srcs = ["parameters.cc"],
    deps = [
        ":type",
        "//common:identifier",
        "//common/constant:manifest",
        "//common/language:type_kind",
        "@com_google_absl//absl/hash",
        "@com_google_absl//absl/container:flat_hash_set",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "pattern",
    hdrs = ["pattern.h"],
    srcs = ["pattern.cc"],
    deps = [
        ":type",
        "//common/constant:category",
        "//common/constant:entry",
        "//common/constant:manifest",
        "//common/language:type_kind",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "pointer",
    hdrs = ["pointer.h"],
    srcs = ["pointer.cc"],
    deps = [
        ":type",
        "//common/constant:category",
        "//common/constant:entry",
        "//common/constant:manifest",
        "//common/language:type_kind",
        "@com_google_absl//absl/container:flat_hash_map",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "primitive",
    hdrs = ["primitive.h"],
    deps = [
        "//common/language:primitive_types",
        "//common/language:type_kind",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "qualified_type",
    hdrs = ["qualified_type.h"],
    deps = [
        ":type",
        ":qualifier",
    ],
)

cc_library(
    name = "qualifier",
    hdrs = ["qualifier.h"],
    deps = [
        ":type",
    ],
)

cc_library(
    name = "refinement",
    hdrs = ["refinement.h"],
    srcs = ["refinement.cc"],
    deps = [
        ":type",
        "//common/language:type_kind",
        "//common:any_value",
        "//common:pattern",
        "@jasmin//jasmin/core:function",
        "@nth_cc//nth/debug",
    ],
)

cc_library(
    name = "type",
    hdrs = ["type.h"],
    srcs = ["type.cc"],
    deps = [
        ":type_contour",
        "//common/constant:manifest",
        "//common/internal:constant_handle",
        "//common/language:type_kind",
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
