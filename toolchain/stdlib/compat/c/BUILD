load("//toolchain:ic_rule.bzl", "ic_library")
load("//toolchain/stdlib/compat/c/internal:generate.bzl", "generated_ic_library")

package(default_visibility = ["//visibility:public"])

ic_library(
    name = "types",
    srcs = [":generate_c_types"],
)

genrule(
    name = "generate_c_types",
    outs = ["types.ic"],
    cmd = "./$(location //toolchain/stdlib/compat/c/internal:c_types_generator) > $@",
    tools = ["//toolchain/stdlib/compat/c/internal:c_types_generator"],
    visibility = ["//visibility:private"],
)

ic_library(
    name = "ctype",
    srcs = ["ctype.ic"],
    deps = [":types"],
)

ic_library(
    name = "errno",
    srcs = ["errno.ic"],
    deps = [":types"],
)

generated_ic_library(
    name = "limits",
    srcs = ["limits.template.ic"],
    includes = ["limits.h"],
    symbols = {
        "CHAR_BIT": "int",
        "SCHAR_MIN": "int",
        "SCHAR_MAX": "int",
        "UCHAR_MAX": "int",
        "CHAR_MIN": "int",
        "CHAR_MAX": "int",
        "MB_LEN_MAX": "int",
        "SHRT_MIN": "int",
        "SHRT_MAX": "int",
        "USHRT_MAX": "unsigned int",
        "INT_MIN": "int",
        "INT_MAX": "int",
        "UINT_MAX": "unsigned int",
        "LONG_MIN": "long",
        "LONG_MAX": "long",
        "ULONG_MAX": "unsigned long",
        "LLONG_MIN": "long long",
        "LLONG_MAX": "long long",
        "ULLONG_MAX": "unsigned long long",
    },
    deps = [],
)

generated_ic_library(
    name = "locale",
    srcs = ["locale.template.ic"],
    includes = ["locale.h"],
    symbols = {
        "LC_ALL": "int",
        "LC_COLLATE": "int",
        "LC_CTYPE": "int",
        "LC_MONETARY": "int",
        "LC_NUMERIC": "int",
        "LC_TIME": "int",
    },
    deps = [],
)

generated_ic_library(
    name = "stdio",
    srcs = ["stdio.template.ic"],
    includes = ["stdio.h"],
    symbols = {
        "BUFSIZ": "int",
        "EOF": "int",
        "FOPEN_MAX": "int",
        "FILENAME_MAX": "int",
        "L_tmpnam": "int",
        "SEEK_CUR": "int",
        "SEEK_END": "int",
        "SEEK_SET": "int",
        "TMP_MAX": "int",
    },
    deps = [":types"],
)

ic_library(
    name = "string",
    srcs = ["string.ic"],
    deps = [":types"],
)

generated_ic_library(
    name = "time",
    srcs = ["time.template.ic"],
    includes = ["time.h"],
    symbols = {
        "CLOCKS_PER_SEC": "int",
        "TIME_UTC": "int",
    },
    deps = [
        ":types",
    ],
)

ic_library(
    name = "test_build",
    srcs = ["test.ic"],
    testonly = True,
    visibility = ["//visibility:private"],
    deps = [
        ":ctype",
        # ":errno",
        # ":limits",
        # ":locale",
        ":string",
        # ":stdio",
        ":types",
        # ":time",
    ],
)
