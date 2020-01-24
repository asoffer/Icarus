package(default_visibility = ['//visibility:public'])
load(':cc_toolchain_config.bzl', 'cc_toolchain_config')

cc_toolchain_suite(
    name = 'cc',
    toolchains = {
        'gcc': ':gcc_toolchain',
        'clang': ':clang_toolchain',
    },
)

filegroup(name = 'empty')

cc_toolchain(
    name = 'gcc_toolchain',
    toolchain_identifier = 'gcc-toolchain',
    toolchain_config = ':gcc_toolchain_config',
    all_files = ':empty',
    compiler_files = ':empty',
    dwp_files = ':empty',
    linker_files = ':empty',
    objcopy_files = ':empty',
    strip_files = ':empty',
    supports_param_files = 0,
)

cc_toolchain_config(
    name = 'gcc_toolchain_config',
    compiler_path = '/usr/bin/g++'
)

cc_toolchain(
    name = 'clang_toolchain',
    toolchain_identifier = 'clang-toolchain',
    toolchain_config = ':clang_toolchain_config',
    all_files = ':empty',
    compiler_files = ':empty',
    dwp_files = ':empty',
    linker_files = ':empty',
    objcopy_files = ':empty',
    strip_files = ':empty',
    supports_param_files = 0,
)

cc_toolchain_config(
    name = 'clang_toolchain_config',
    compiler_path = '/usr/bin/clang++'
)
