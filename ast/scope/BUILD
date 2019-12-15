package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "scope",
    deps = [
        "//ast:ast_fwd",
        "//base:cast",
        "//base:debug",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/types:span"
    ],
    test_deps = None,
)

cc_lib(
    name = "decl",
    deps = [":scope"],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "scope_lit",
    deps = [":scope"],
    header_only = True,
    test_deps = None,
)


cc_lib(
    name = "exec",
    deps = [
        ":scope",
        ":fn",
    ],
    test_deps = None,
)


cc_lib(
    name = "fn",
    deps = [
        ":scope",
        "//base:bag",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "module",
    deps = [
        ":fn",
        ":scope",
    ],
    header_only = True,
    test_deps = None,
)
