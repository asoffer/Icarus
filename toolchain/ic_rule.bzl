IcarusInfo = provider(
    "Information needed to compile and link an Icarus binary",
    fields = ["icms", "icmods"],
)

def _tooling_transition_impl(settings, attr):
    return {
#         "//command_line_option:compilation_mode": "opt",
#         "//command_line_option:cpu": "clang",
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


def _unique_id(label):
    return str(label)


def _dotted_path(label):
    path = str(label)
    if path.startswith("@"):
        path = path[1:]
    if path.startswith("//"):
        path = path[2:]
    if path.startswith("toolchain/stdlib"):
        path = "std" + path[len("toolchain/stdlib"):]
    path = path.replace("/", ".").replace(":", ".")
    return path


def _module_map_file(ctx, icm_deps):
    module_map = ctx.actions.declare_file(ctx.label.name + ".icmod")
    ctx.actions.write(
        output = module_map,
        content = '\n'.join([
            "{id}\n{name}\n{icm}".format(
                id = dep.id,
                name = dep.name,
                icm = dep.icm.short_path)
            for dep in icm_deps.to_list()
        ])
    )
    return module_map


def _compile(ctx, icm_file, icm_deps):
    module_map_file = _module_map_file(ctx, icm_deps)

    if len(ctx.attr.srcs) != 1:
        fail("library rules must have exactly one file in 'srcs'.")
    src = ctx.attr.srcs[0]

    src_file = src.files.to_list()[0]

    ctx.actions.run(
        inputs = depset(
            [module_map_file] + [dep.icm for dep in icm_deps.to_list()],
            transitive = [src.files for src in ctx.attr.srcs]
        ),
        outputs = [icm_file],
        arguments = [
            "--source={}".format(src_file.short_path),
            "--module_identifier={}".format(_unique_id(ctx.label)),
            "--output={}".format(icm_file.short_path),
            "--module_map_file={}".format(module_map_file.short_path),
        ],
        progress_message = "Compiling {}".format(ctx.label.name),
        executable = ctx.attr._compile[0][DefaultInfo].files_to_run.executable,
    )
    return module_map_file
 

def _ic_binary_impl(ctx):
    icm_deps = depset(transitive = [d[IcarusInfo].icms for d in ctx.attr.deps])

    icm_file = ctx.actions.declare_file("{label}.icm".format(
        label = ctx.label.name
    ))

    module_map_file = _compile(ctx, icm_file, icm_deps)

    runfiles = ctx.runfiles(files = (
        [icm_file, ctx.executable._run_bytecode, module_map_file] +
        [d.icm for d in icm_deps.to_list()]))
    ctx.actions.write(
        output = ctx.outputs.executable,
        is_executable = True,
        content = """
        echo "{executable} --input={icm} --module_map_file={mod}"
        {executable} --input={icm} --module_map_file={mod}
        """.format(
            executable = ctx.executable._run_bytecode.short_path,
            icm = icm_file.short_path,
            mod = module_map_file.short_path,
        )
    )

    return [DefaultInfo(runfiles = runfiles)]


ic_binary = rule(
    implementation = _ic_binary_impl,
    exec_groups = {
        "run_bytecode": exec_group(copy_from_rule = True),
    },
    attrs = {
        "srcs": attr.label_list(allow_files = [".ic"]),
        "deps": attr.label_list(providers = [IcarusInfo]),
        "_compile": attr.label(
            default = Label("//toolchain:compile"),
            allow_single_file = True,
            executable = True,
            cfg = _tooling_transition,
        ),
        "_run_bytecode": attr.label(
            default = Label("//toolchain:run_bytecode"),
            allow_single_file = True,
            executable = True,
            cfg = _tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
    executable = True,
)


def _ic_library_impl(ctx):
    icm_deps = depset(transitive = [d[IcarusInfo].icms for d in ctx.attr.deps])

    icm_file = ctx.actions.declare_file("{label}.icm".format(
        label = ctx.label.name
    ))

    info = struct(
        id = _unique_id(ctx.label),
        name = _dotted_path(ctx.label),
        icm = icm_file,
    )

    module_map_file = _compile(ctx, icm_file, icm_deps)

    return [
        IcarusInfo(
            icms = depset([info], transitive = [icm_deps]),
            icmods = depset([module_map_file], transitive = [
                d[IcarusInfo].icmods for d in ctx.attr.deps
            ]),
        ),
    ]


ic_library = rule(
    implementation = _ic_library_impl,
    exec_groups = {
        "run_bytecode": exec_group(copy_from_rule = True),
    },
    attrs = {
        "srcs": attr.label_list(allow_files = [".ic"]),
        "deps": attr.label_list(providers = [IcarusInfo]),
        "_compile": attr.label(
            default = Label("//toolchain:compile"),
            allow_single_file = True,
            executable = True,
            cfg = _tooling_transition,
        ),
        "_run_bytecode": attr.label(
            default = Label("//toolchain:run_bytecode"),
            allow_single_file = True,
            executable = True,
            cfg = _tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
)
