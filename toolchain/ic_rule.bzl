IcarusInfo = provider(
    "Information needed to compile and link an Icarus binary",
    fields = {
        "sources": "depset of Files from compilation.",
    },
)

def _interpreter_transition_impl(settings, attr):
    return {
        "//command_line_option:compilation_mode": "opt",
        "//command_line_option:cpu": "clang",
    }

_interpreter_transition = transition(
    implementation = _interpreter_transition_impl,
    inputs = [
        "//command_line_option:compilation_mode",
        "//command_line_option:cpu", 
    ],
    outputs = [
        "//command_line_option:compilation_mode",
        "//command_line_option:cpu", 
    ],
)


def _ic_library_impl(ctx):
    return [IcarusInfo(
        sources = depset(
            ctx.attr.srcs,
            transitive = ([dep[IcarusInfo].sources for dep in ctx.attr.deps] + 
                          getattr(ctx.attr, "_implicit_deps", [])),
        ),
    )]


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
    },
)


# A rule for building low-level libraries. This is identical to `ic_library`
# except that it has no implicit depenedncies. Any dependency that would be
# implicit is `ic_library` must be defined with `ic_low_level_library`.
ic_low_level_library = rule(
    implementation = _ic_library_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = [".ic"]),
        "deps": attr.label_list(providers = [IcarusInfo]),
    },
)


def _ic_interpret_impl(ctx):
    executable = ctx.actions.declare_file(ctx.label.name)

    data_deps_attribute = getattr(ctx.attr, "data", [])
    data_deps = depset(transitive = [d[DefaultInfo].files for d in data_deps_attribute]).to_list()
    target_deps = ctx.attr.deps + getattr(ctx.attr, "_implicit_deps", [])
    input_file_targets = depset(transitive = [dep[IcarusInfo].sources for dep in target_deps])
    input_files = depset(data_deps,
                         transitive = [f.files for f in input_file_targets.to_list()])

    ctx.actions.write(
        output = executable,
        content = ('./' +
                   ctx.attr._interpreter[0][DefaultInfo].files_to_run.executable.short_path +
                   ' ' + 
                   ' '.join([f.path for f in ctx.attr.src[DefaultInfo].files.to_list()]) + 
                   ' --module_paths=stdlib'),
        is_executable = True,
    )

    runfiles = ctx.runfiles(
        files = ctx.attr.src[DefaultInfo].files.to_list(),
        transitive_files = depset(
            transitive = [input_files]
        ),
    )
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
            cfg = _interpreter_transition,
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
            cfg = _interpreter_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
)
