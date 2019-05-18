def icarus_ast_node(name, deps):
    native.cc_library(
        name = name,
        defines = select({
            "//:config_emit_ir": ["ICARUS_VISITOR_EMIT_IR"],
            "//conditions:default": [],
        }),
        hdrs = [name + ".h"],
        deps = deps,
        alwayslink = True,
    )
