package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib")

cc_lib(
    name = "basic",
    deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "call",
    deps = [
        ":util",
        "//ir:basic_block",
        "//ir:reg",
        "//ir:results",
        "//type:util",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None,
)

cc_lib(
    name = "cast",
    deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "jumps",
    deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
        "@com_google_absl//absl/types:span",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "load",
    deps = [
        ":util",
        "//ir:basic_block",
        "//type:util",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "misc",
    deps = [
        ":util",
        "//base:debug",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "phi",
    deps = [
        ":util",
        "//ir:basic_block",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "print",
    deps = [
        ":util",
        "//ir:basic_block",
    ],
    test_deps = None,
)

cc_lib(
    name = "register",
    deps = [
        ":util",
        "//ir:basic_block",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "return",
    deps = [
        ":util",
        "//ir:basic_block",
        "//type:jump",
        "@com_google_absl//absl/strings",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "scope",
    deps = [
        ":util",
        "//base:stringify",
        # TODO "//compiler:compiler",
        "//ir:basic_block",
        "//ir:block_def",
        "@com_google_absl//absl/strings",
    ],
    test_deps = None,
)

cc_lib(
    name = "store",
    deps = [
        ":util",
        "//ir:basic_block",
    ],
    header_only = True,
    test_deps = None,
)

cc_lib(
    name = "types",
    deps = [
        ":util",
        "//ir:basic_block",
        "//type:enum",
        "//type:flags",
        "//type:pointer",
        "//type:variant",
        "@com_google_absl//absl/random:random",
        "@com_google_absl//absl/random:distributions",
    ],
    test_deps = None,
)

cc_lib(
    name = "util",
    deps = [
        "//ir:builder",
        "//ir:values",
        "//base:tag",
        "//type:primitive",
        "//type:util",
    ],
    header_only = True,
    test_deps = None,
)

CMDS = [
    ":basic",
    ":call",
    ":cast",
    ":jumps",
    ":load",
    ":misc",
    ":phi",
    ":print",
    ":register",
    ":return",
    ":scope",
    ":store",
    ":types",
]

cc_lib(
    name = "execute",
    deps = [
        "//backend:exec",
        "//base:untyped_buffer",
        "//compiler:compiler",
        "//ir:addr",
    ] + CMDS,
    header_only = True,
    test_deps = None,
)
