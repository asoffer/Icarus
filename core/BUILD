package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "alignment",
    deps = [],
    header_only = True,
    test_deps = None,
)
cc_lib(
    name = "bytes",
    deps = [],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "arch",
    deps = [
        ":alignment",
        ":bytes",
        "//ir:addr",
        "//ir:any_func",
    ],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "builtin",
    textual_hdrs = ["builtin.xmacro.h"],
    deps = ["//base:debug"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "fn_args",
    deps = [
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "fn_params",
    deps = [
        ":fn_args",
        "//base:debug",
        "//base:macros",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
    header_only = True,
    test_deps = [],
)

cc_lib(
    name = "ordered_fn_args",
    deps = [
        ":fn_args",
        "//base:stringify",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
    header_only = True,
    test_deps = [],
)
