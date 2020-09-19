package(default_visibility = ["//visibility:public"])

cc_library(
    name = "scope",
    hdrs = ["scope.h"],
    srcs = ["scope.cc"],
    deps = [
        "//ast:ast_fwd",
        "//base:cast",
        "//base:debug",
        "//base:log",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/types:span"
    ],
)

cc_library(
    name = "decl",
    hdrs = ["decl.h"],
    deps = [":scope"],
)

cc_library(
    name = "scope_lit",
    hdrs = ["scope_lit.h"],
    deps = [":scope"],
)


cc_library(
    name = "exec",
    hdrs = ["exec.h"],
    srcs = ["exec.cc"],
    deps = [
        ":scope",
        ":fn",
    ],
)


cc_library(
    name = "fn",
    hdrs = ["fn.h"],
    deps = [
        ":scope",
        "@com_google_absl//absl/types:span"
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    deps = [
        ":fn",
        ":scope",
    ],
)
