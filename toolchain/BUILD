package(default_visibility = ['//visibility:public'])
filegroup(name = 'empty')

cc_toolchain_suite(
    name = 'gcc_toolchain_suite',
    toolchains = {
        'gcc': ':gcc_toolchain'
    }
)

cc_toolchain(
    name = 'gcc_toolchain',
    toolchain_identifier = 'gcc-toolchain',
    all_files = ':empty',
    compiler_files = ':empty',
    cpu = 'gcc_cpu',
    strip_files = ':empty',
    objcopy_files = ':empty',
    linker_files  = ':empty',
    dwp_files = ':empty',
)
