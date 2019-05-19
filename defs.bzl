_VISITOR_DEFINES = {
    "compile": ["ICARUS_VISITOR_EMIT_IR"],
    "format": [],
}

def configured_dep(dep, cfg):
  if dep[0] == "@":
      return dep
  else:
      return dep + "-" + cfg

def cc_lib_target(name, intf_deps = [], impl_deps = None,
                  test_deps = None, test_data = [], 
                  extra_hdrs = [], extra_srcs = [], **kwargs):
    for cfg, defs in _VISITOR_DEFINES.items():
        intf_name = configured_dep(name, cfg)
        impl_name = configured_dep(name + "-impl", cfg)
        test_name = configured_dep(name + "-test", cfg)
        if intf_deps != None:
            native.cc_library(
                name = intf_name,
                hdrs = [name + ".h"] + extra_hdrs,
                defines = defs,
                deps = [configured_dep(dep, cfg) for dep in intf_deps],
                alwayslink = True,
                **kwargs)

        if impl_deps != None:
            native.cc_library(
                name = impl_name,
                srcs = [name + ".cc"] + extra_srcs,
                defines = defs,
                deps = (([intf_name] if intf_deps != None else []) +
                        [configured_dep(dep, cfg) for dep in impl_deps]),
                alwayslink = True,
                **kwargs)

        if test_deps != None:
            native.cc_test(
                name = test_name,
                defines = defs,
                srcs = [name + "_test.cc"],
                deps = ([configured_dep("//test:test", cfg),
                         impl_name if impl_deps != None else intf_name] +
                        [configured_dep(dep, cfg) for dep in test_deps]),
                data = test_data,
                **kwargs)

def cc_group_target(name, deps, **kwargs):
    for cfg, defs in _VISITOR_DEFINES.items():
        native.cc_library(
            name = configured_dep(name, cfg),
            deps = [configured_dep(dep, cfg) for dep in deps],
            defines = defs,
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
