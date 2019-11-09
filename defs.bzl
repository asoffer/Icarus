def configuration(name, defines = []):
    return {
        "name": name,
        "defines": defines
    }


_VISITOR_DEFINES = {
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

def cc_lib(name, deps, test_deps = [], header_only = False, test_data = None, **kwargs):
  native.cc_library(
      name = name,
      hdrs = [name + ".h"],
      srcs = [] if header_only else [name + ".cc"],
      deps = deps,
      **kwargs
  )

  if test_deps != None:
    native.cc_test(
        name = name + "-test",
        srcs = [name + "_test.cc"],
        deps = ["//test", ":" + name] + test_deps,
        data = test_data
    )


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
