package(default_visibility = ['//visibility:public'])
load(':cc_toolchain_config.bzl', 'cc_toolchain_config')

cc_toolchain_suite(
    name = 'cc',
    toolchains = {
        'k8': ':k8_toolchain',
    },
)

filegroup(name = 'empty')

cc_toolchain(
    name = 'k8_toolchain',
    toolchain_identifier = 'k8-toolchain',
    toolchain_config = ':k8_toolchain_config',
    all_files = ':empty',
    compiler_files = ':empty',
    dwp_files = ':empty',
    linker_files = ':empty',
    objcopy_files = ':empty',
    strip_files = ':empty',
    supports_param_files = 0,
)

cc_toolchain_config(
    name = 'k8_toolchain_config',
    compiler_path = "/usr/bin/clang-16",
    include_dirs = [
        "/usr/lib",
        "/usr/include",
        "/usr/local/include",
    ],
    warnings = [
        "-Wall",
        "-Werror",
        "-Wextra",
        "-Wno-sign-compare",
        "-Wno-unused-parameter",
        "-Wno-unused-variable",
        "-Wno-unused-command-line-argument",
        "-Wno-deprecated-non-prototype",
        "-Wuninitialized",
    ]
)
