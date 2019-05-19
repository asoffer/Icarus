load("//:defs.bzl", "cc_lib_target")

def icarus_ast_node(name, deps):
    cc_lib_target(name = name, intf_deps = deps, impl_deps = None, test_deps = None)
