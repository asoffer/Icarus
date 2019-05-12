def icarus_ast_node(name, deps):
    native.cc_library(
        name = name,
        hdrs = [name + ".h"],
        deps = deps,
        alwayslink = True,
    )
