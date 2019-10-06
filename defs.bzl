def configuration(name, defines = []):
    return {
        "name": name,
        "defines": defines + [
            "ICARUS_AST_VISITOR_DEPENDENCIES=\\\"{}/ast_dependencies.h\\\"".format(name),
            "ICARUS_TYPE_VISITOR_DEPENDENCIES=\\\"{}/type_dependencies.h\\\"".format(name),
        ]
    }


_VISITOR_DEFINES = {
    "compile": configuration("compiler"),
    "format": configuration("format"),
    #TODO Figure out how to get rid of this define.
    "match": configuration("match", defines = ["ICARUS_MATCHER"])
}

def configured_dep(dep, cfg):
  if dep[0] == "@":
      return dep
  else:
      return dep + "-" + cfg

def make_deps(deps, cfg):
    if str(type(deps)) == "list":
        return [configured_dep(dep, cfg) for dep in deps]
    elif str(type(deps)) == "dict":
        if cfg in deps:
            return [configured_dep(dep, cfg) for dep in deps[cfg]]
        else:
            return []
    else:
        fail()

def ast_dependency(cfg):
    return "//{}:ast_dependencies-{}".format(_VISITOR_DEFINES[cfg]["name"], cfg)

def type_dependency(cfg):
    return "//{}:type_dependencies-{}".format(_VISITOR_DEFINES[cfg]["name"], cfg)

def cc_lib_target(name, intf_deps = [], impl_deps = None,
                  test_deps = None, test_data = [], 
                  extra_hdrs = [], extra_srcs = [],
                  cfgs = None, **kwargs):
    for cfg, defs in _VISITOR_DEFINES.items():
        if cfgs != None and cfg not in cfgs:
            continue

        intf_name = configured_dep(name, cfg)
        impl_name = configured_dep(name + "-impl", cfg)
        test_name = configured_dep(name + "-test", cfg)
        if intf_deps != None:
            native.cc_library(
                name = intf_name,
                hdrs = [name + ".h"] + extra_hdrs,
                defines = defs["defines"],
                deps = [
                    configured_dep(dep, cfg) if str(type(dep)) == "string" else dep(cfg)
                    for dep in intf_deps],
                alwayslink = True,
                **kwargs)

        if impl_deps != None:
            native.cc_library(
                name = impl_name,
                srcs = [name + ".cc"] + extra_srcs,
                defines = defs["defines"],
                deps = (([intf_name] if intf_deps != None else []) +
                        [configured_dep(dep, cfg) if str(type(dep)) == "string" else dep(cfg)
                         for dep in impl_deps]),
                alwayslink = True,
                **kwargs)

        if test_deps != None:
            native.cc_test(
                name = test_name,
                defines = defs["defines"],
                srcs = [name + "_test.cc"],
                deps = (["//test:test",
                         impl_name if impl_deps != None else intf_name] +
                        [configured_dep(dep, cfg) if str(type(dep)) == "string" else dep(cfg)
                         for dep in test_deps]),
                data = test_data,
                **kwargs)

def icarus_method(name,
                  intf_deps = None,
                  impl_deps = None,
                  deps = [],
                  gen = True):
    native.cc_library(
        name = name + "-xmacro",
        textual_hdrs = [name + ".xmacro.h"],
        deps = deps)

    if gen:
        cc_lib_target(name,
                      intf_deps = intf_deps,
                      impl_deps = impl_deps)


    for cfg, defs in _VISITOR_DEFINES.items():
        native.alias(name = configured_dep(name + "-xmacro", cfg), actual = name + "-xmacro")


def cc_component(name, intf_deps = [], impl_deps = None,
                 test_deps = None, test_data = [], 
                 extra_hdrs = [], **kwargs):
    if intf_deps != None:
        native.cc_library(
            name = name,
            hdrs = [name + ".h"] + extra_hdrs,
            deps = intf_deps,
            alwayslink = True,
            **kwargs)

    if impl_deps != None:
        native.cc_library(
            name = name + "-impl",
            srcs = [name + ".cc"],
            deps = ([name] if intf_deps != None else []) + impl_deps,
            alwayslink = True,
            **kwargs)

    if test_deps != None:
        native.cc_test(
            name = name + "-test",
            srcs = [name + "_test.cc"],
            deps = (["//test:test",
                     name + "-impl" if impl_deps != None else name] 
                    + test_deps),
            data = test_data,
            **kwargs)

    for cfg, defs in _VISITOR_DEFINES.items():
        if intf_deps != None:
            native.alias(
                name = configured_dep(name, cfg),
                actual = name)

        if impl_deps != None:
            native.alias(
                name = configured_dep(name + "-impl", cfg),
                actual = name + "-impl")

def cc_group_target(name, deps, cfgs = None, hdrs = [], srcs = [], **kwargs):
    for cfg, defs in _VISITOR_DEFINES.items():
        if cfgs != None and cfg not in cfgs:
            continue

        native.cc_library(
            name = configured_dep(name, cfg),
            hdrs = hdrs,
            srcs = srcs,
            deps = make_deps(deps, cfg),
            defines = defs["defines"],
            alwayslink = True,
            **kwargs)
