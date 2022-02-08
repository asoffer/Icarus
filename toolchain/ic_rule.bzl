IcarusInfo = provider(
    "Information needed to compile and link an Icarus binary",
    fields = {
        "sources": """
            The source files defining this module.
        """,
        "module_map": """
            A module-map dictionaries for dependent modules that
            are not precompiled
        """,
        "precompiled": """
            A boolean value indicating whether the output file is a precompiled
            module or a source file.
        """,
    },
)

def _tooling_transition_impl(settings, attr):
    return {
        "//command_line_option:compilation_mode": "opt",
        "//command_line_option:cpu": "clang",
    }

_tooling_transition = transition(
    implementation = _tooling_transition_impl,
    inputs = [
        "//command_line_option:compilation_mode",
        "//command_line_option:cpu", 
    ],
    outputs = [
        "//command_line_option:compilation_mode",
        "//command_line_option:cpu", 
    ],
)

def _module_mapping(deps):
    """
    Given a list of dependencies `deps`, returns a map keyed on source file
    names where the associated value is the corresponding precompiled module
    file built from that source.
    """
    # TODO: Take transitive into account. E.g. //stdlib target
    return {
        target[IcarusInfo].sources[0]:  target[DefaultInfo].files.to_list()[0]
        for target in deps if target[IcarusInfo].precompiled
    }


def _merge_module_maps(maps):
    """
    Given a collection of module maps (dictionaries), returns a module map consisting of the 
    entries of each of the maps.
    """
    result = {}
    for m in maps:
        for k, v in m.items():
            result[k] = v

    return result


def _module_map_file(ctx, mapping, run):
    module_map = ctx.actions.declare_file(ctx.label.name + ".module_map")
    ctx.actions.write(
        output = module_map,
        content = '\n'.join([
            "{}:{}".format(src.files.to_list()[0].path, 
                           icm.short_path if run else icm.path)
            for (src, icm) in mapping.items()
        ])
    )
    return module_map


def _ic_library_impl(ctx):
    if ctx.attr.precompile and len(ctx.attr.srcs) != 1:
        fail("Precompiled targets with more than one source file are currently supported.")

    target_deps = ctx.attr.deps + getattr(ctx.attr, "_implicit_deps", [])
    module_map = _module_mapping(target_deps)

    if ctx.attr.precompile:
        output = ctx.actions.declare_file(ctx.label.name + ".icm")

        module_map_file = _module_map_file(ctx, module_map, False)

        # TODO: Support data dependencies.
        ctx.actions.run(
            inputs = depset(
                         [module_map_file] + 
                         [icm for (src, icm) in module_map.items()],
                         transitive = [src.files for src in ctx.attr.srcs],
                     ),
            outputs = [output],
            arguments = ["--byte_code={}".format(output.path),
                         '--module_map=' + module_map_file.path,
                         "--"] + [
                f.path for f in depset(transitive = [f.files for f in ctx.attr.srcs]).to_list()],
            progress_message = "Compiling {}".format(ctx.label.name),
            executable = ctx.attr._compile[0][DefaultInfo].files_to_run.executable,
        )

        outputs = depset([output])
    else:
        deps = ctx.attr.deps + getattr(ctx.attr, "_implicit_deps", [])
        outputs = depset(
            transitive = [src.files for src in ctx.attr.srcs] +
            [dep.files for dep in target_deps]
        )


    return [
        IcarusInfo(
            sources = ctx.attr.srcs,
            module_map = module_map,
            precompiled = ctx.attr.precompile,
        ),
        DefaultInfo(files = outputs),
    ]


# A rule describing the dependencies of an icarus library. Each such library has
# an implicit dependency on `//stdlib`. Use `ic_low_level_library` if you wish to
# build a rule with no implicit dependencies. Note that low-level libraries may
# still depend on the standard library. They just need to declare those
# dependencies explicitly.
ic_library = rule(
    implementation = _ic_library_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = [".ic"]),
        "deps": attr.label_list(providers = [IcarusInfo]),
        "_implicit_deps": attr.label_list(default = [Label("//stdlib")]),
        "_compile": attr.label(
            default = Label("//:icarus"),
            allow_single_file = True,
            executable = True,
            cfg = _tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
)


# A rule for building low-level libraries. This is identical to `ic_library`
# except that it has no implicit depenedncies. Any dependency that would be
# implicit in `ic_library` must be defined with `ic_low_level_library`.
ic_low_level_library = rule(
    implementation = _ic_library_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = [".ic"]),
        "deps": attr.label_list(providers = [IcarusInfo]),
        "precompile": attr.bool(
            default = True,
            doc = """
            A temporary flag defaulting to True that indicates whether the
            library should be pre-compiled or needs to be reprocessed for each
            module containing it. This is to be used while module precompilation
            is only partially supported.
            """),
        "_compile": attr.label(
            default = Label("//:icarus"),
            allow_single_file = True,
            executable = True,
            cfg = _tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
)


def _ic_interpret_impl(ctx):
    target_deps = ctx.attr.deps + getattr(ctx.attr, "_implicit_deps", [])

    srcs = ctx.attr.src[DefaultInfo].files.to_list()

    if len(srcs) != 1: 
        fail("Only targets with exactly one source file are currently supported.")
    src = srcs[0].path

    interpreter_info = ctx.attr._interpreter[0][DefaultInfo]
    interpreter = interpreter_info.files_to_run.executable.short_path

    cmd_template = '''
    #!/bin/bash
    ./{interpreter} {src} --module_map={module_map} {module_paths}
    '''
    module_map = _merge_module_maps([
        t[IcarusInfo].module_map for t in target_deps if IcarusInfo in t
    ] + [_module_mapping(target_deps)])
    module_map_file = _module_map_file(ctx, module_map, True)

    executable = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.write(
        output = executable,
        content = cmd_template.format(
            interpreter = interpreter,
            src = src,
            module_map = module_map_file.short_path,
            module_paths = "--module_paths=stdlib",
        ),
        is_executable = True,
    )

    runfile_deps = depset(
        srcs + [module_map_file] + [icm for (src, icm) in module_map.items()],
        transitive = ([t[DefaultInfo].files for t in target_deps] +
                      [d.files for d in getattr(ctx.attr, "data", [])])
    )

    runfiles = ctx.runfiles(files = runfile_deps.to_list())
    runfiles = runfiles.merge(ctx.attr._interpreter[0].default_runfiles)

    return [
        DefaultInfo(executable = executable, runfiles = runfiles),
    ]

# TODO: Eventually we will want to replace with with `ic_binary` and use an
# aspect to do the interpretation.
ic_interpret = rule(
    implementation = _ic_interpret_impl,
    executable = True,
    attrs = {
        "src": attr.label(allow_single_file = [".ic"], mandatory = True),
        "data": attr.label_list(providers = [DefaultInfo]),
        "deps": attr.label_list(providers = [IcarusInfo]),
        "_implicit_deps": attr.label_list(default = [Label("//stdlib")]),
        "_interpreter": attr.label(
            default = Label("//:interpreter"),
            allow_single_file = True,
            executable = True,
            cfg = _tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
)


ic_low_level_interpret = rule(
    implementation = _ic_interpret_impl,
    executable = True,
    attrs = {
        "src": attr.label(allow_single_file = [".ic"], mandatory = True),
        "data": attr.label_list(providers = [DefaultInfo]),
        "deps": attr.label_list(providers = [IcarusInfo]),
        "_interpreter": attr.label(
            default = Label("//:interpreter"),
            allow_single_file = True,
            executable = True,
            cfg = _tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
)
