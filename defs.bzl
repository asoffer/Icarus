def configuration(ast_methods, ast_deps, type_methods, type_deps, defines = []):
    return {
        "defines": defines + [
            "ICARUS_AST_VISITOR_METHODS=\\\"{}\\\"".format(ast_methods),
            "ICARUS_AST_VISITOR_DEPENDENCIES=\\\"{}\\\"".format(ast_deps),
            "ICARUS_TYPE_VISITOR_METHODS=\\\"{}\\\"".format(type_methods),
            "ICARUS_TYPE_VISITOR_DEPENDENCIES=\\\"{}\\\"".format(type_deps),
        ]
    }

_VISITOR_DEFINES = {
    "compile": configuration(
        ast_methods = "compiler/ast_methods.xmacro.h",
        ast_deps = "compile_ast_dependencies.h",
        type_methods = "visitor/type_visitors.xmacro.h",
        type_deps = "compile_type_dependencies.h",
        defines = ["ICARUS_VISITOR_EMIT_IR"]),
    "format": configuration(
        ast_methods = "format/ast_methods.xmacro.h",
        ast_deps = "format_ast_dependencies.h",
        type_methods = "visitor/type_visitors.xmacro.h",
        type_deps = "format_type_dependencies.h",
        ),
    "match": configuration(
        ast_methods = "visitor/xvisitors.xmacro.h",
        ast_deps = "match_ast_dependencies.h",
        type_methods = "visitor/type_visitors.xmacro.h",
        type_deps = "match_type_dependencies.h",
        defines = ["ICARUS_VISITOR_EMIT_IR", "ICARUS_MATCHER"])
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
                deps = [configured_dep(dep, cfg) for dep in intf_deps],
                alwayslink = True,
                **kwargs)

        if impl_deps != None:
            native.cc_library(
                name = impl_name,
                srcs = [name + ".cc"] + extra_srcs,
                defines = defs["defines"],
                deps = (([intf_name] if intf_deps != None else []) +
                        [configured_dep(dep, cfg) for dep in impl_deps]),
                alwayslink = True,
                **kwargs)

        if test_deps != None:
            native.cc_test(
                name = test_name,
                defines = defs["defines"],
                srcs = [name + "_test.cc"],
                deps = (["//test:test",
                         impl_name if impl_deps != None else intf_name] +
                        [configured_dep(dep, cfg) for dep in test_deps]),
                data = test_data,
                **kwargs)

def icarus_ast_method(name,
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
