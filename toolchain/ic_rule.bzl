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
    return [
        IcarusInfo(
            sources = depset(
                depset([], transitive = [src.files for src in ctx.attr.srcs]).to_list(),
                transitive = [dep[IcarusInfo].sources for dep in ctx.attr.deps],
            ),
        ),
    ]

ic_library = rule(
    implementation = _ic_library_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = [".ic"]),
        "deps": attr.label_list(providers = [IcarusInfo]),
    },
)


def _ic_interpret_impl(ctx):
    executable = ctx.actions.declare_file(ctx.label.name)

    ctx.actions.write(
        output = executable,
        content = ('./' +
                   ctx.attr._interpreter[0][DefaultInfo].files_to_run.executable.short_path +
                   ' ' + 
                   ' '.join([f.path for f in ctx.attr.src[DefaultInfo].files.to_list()]) + 
                   ' --module_paths=stdlib'),
        is_executable = True,
    )

    data_deps = depset()
    if hasattr(ctx.attr, "data"):
        data_deps = depset([src for f in depset(ctx.attr.data).to_list() for src in f.files.to_list()])

    runfiles = ctx.runfiles(
        files = ctx.attr.src[DefaultInfo].files.to_list(),
        transitive_files = depset(
            transitive = [dep[IcarusInfo].sources for dep in ctx.attr.deps] + [data_deps]
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
