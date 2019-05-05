def cc_lib(**kwargs):
  native.cc_library(alwayslink = True, **kwargs)

def cc_target(name, intf_deps = None, impl_deps = [], **kwargs):
    native.cc_library(
        name = name,
        hdrs = [name + ".h"],
        deps = intf_deps,
        **kwargs)

    native.cc_library(
        name = name + "-impl",
        srcs = [name + ".cc"],
        deps = [name] + impl_deps,
        alwayslink = True,
        **kwargs)

def sources():
    native.filegroup(
        name = "sources",
        srcs = native.glob(
            include=["*.cc", "**/*.cc"],
            exclude=["*_test.cc", "**/*_test.cc"],
        ),
    )
