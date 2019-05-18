def cc_lib(**kwargs):
  native.cc_library(alwayslink = True, **kwargs)

def cc_target(name, intf_deps = None, impl_deps = [], **kwargs):
    native.cc_library(
        name = name,
        defines = select({
            "//:config_emit_ir": ["ICARUS_VISITOR_EMIT_IR"],
            "//conditions:default": [],
        }),
        hdrs = [name + ".h"],
        deps = intf_deps,
        **kwargs)

    native.cc_library(
        name = name + "-impl",
        defines = select({
            "//:config_emit_ir": ["ICARUS_VISITOR_EMIT_IR"],
            "//conditions:default": [],
        }),
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
