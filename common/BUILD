package(default_visibility = ["//visibility:public"])

cc_library(
    name = "debug",
    hdrs = ["debug.h"],
)

cc_library(
    name = "errno",
    hdrs = ["errno.h"],
)

cc_test(
    name = "errno_test",
    srcs = ["errno_test.cc"],
    deps = [
        ":errno",
        "@asoffer_nth//nth/test:main",
    ]
)

cc_library(
    name = "file",
    hdrs = ["file.h"],
    srcs = ["file.cc"],
    deps = [
        ":errno",
        "@asoffer_nth//nth/debug",
        "@asoffer_nth//nth/io:file",
        "@asoffer_nth//nth/io:file_path",
    ],
)

cc_library(
    name = "identifier",
    hdrs = ["identifier.h"],
    deps = [
        ":strong_identifier_type",
        "@asoffer_nth//nth/container:flyweight_set",
    ],
)

cc_library(
    name = "integer",
    hdrs = ["integer.h"],
    deps = [
        ":strong_identifier_type",
        "@asoffer_nth//nth/container:flyweight_set",
    ],
)

cc_test(
    name = "integer_test",
    srcs = ["integer_test.cc"],
    deps = [
        ":integer",
        "@asoffer_nth//nth/test:main",
    ]
)

cc_library(
    name = "module_id",
    hdrs = ["module_id.h"],
    deps = [
        "@asoffer_jasmin//jasmin/core:value",
    ],
)

cc_library(
    name = "module_map",
    hdrs = ["module_map.h"],
    srcs = ["module_map.cc"],
    deps = [
        ":module_id",
        "@asoffer_nth//nth/container:flyweight_set",
    ],
)

cc_library(
    name = "string",
    hdrs = ["string.h"],
    deps = [
        "@asoffer_nth//nth/io:string_printer",
        "@asoffer_nth//nth/strings:interpolate",
        "@asoffer_nth//nth/strings/format:universal",
    ],
)

cc_library(
    name = "strong_identifier_type",
    hdrs = ["strong_identifier_type.h"],
)

cc_library(
    name = "strong_index_type",
    hdrs = ["strong_index_type.h"],
)

cc_library(
    name = "resources",
    hdrs = ["resources.h"],
    deps = [
        ":module_map",
        "//type",
    ],
)

cc_library(
    name = "slice",
    hdrs = ["slice.h"],
)
