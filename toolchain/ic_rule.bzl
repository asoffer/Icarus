load("//toolchain/internal:transitions.bzl", "ic_tooling_transition")

IcarusInfo = provider(
    "Information needed to compile and link an Icarus binary",
    fields = ["srcs", "icms", "mods", "lbls"],
)


def _unique_id(label):
    return str(label)


def _dotted_path(path):
    if path.endswith(".icm"):
        path = path[:-4]
    if path.startswith("toolchain/stdlib"):
        path = "std" + path[len("toolchain/stdlib"):]
    path = path.replace("/", ".").replace(":", ".")
    return path


def _module_map_file(ctx, module_map_file, short):
    lbls = depset(transitive = [d[IcarusInfo].lbls for d in ctx.attr.deps]).to_list()
    ctx.actions.write(
        output = module_map_file,
        content = '\n'.join([
            "{id}\n{name}\n{icm}".format(
                id = _unique_id(lbl),
                name = _dotted_path(icm.short_path),
                icm = icm.short_path if short else icm.path)
            for (lbl, icm) in lbls
        ])
    )


def _compile_one(ctx, icm_file, icm_deps, module_map_file, short):
    _module_map_file(ctx, module_map_file, short)

    if len(ctx.attr.srcs) != 1:
        fail("library rules must have exactly one file in 'srcs'.")
    src = ctx.attr.srcs[0]

    src_file = src.files.to_list()[0]
    ctx.actions.run(
        inputs = depset([module_map_file], transitive = [src.files, icm_deps]),
        outputs = [icm_file],
        arguments = [
            "--source={}".format(src_file.short_path),
            "--module_identifier={}".format(_unique_id(ctx.label)),
            "--output={}".format(icm_file.path),
            "--module_map_file={}".format(module_map_file.path),
        ],
        progress_message = "Compiling {}".format(ctx.label.name),
        executable = ctx.attr._compile[0][DefaultInfo].files_to_run.executable,
    )
    return module_map_file
 

def _ic_library_impl(ctx):
    src_deps = depset(transitive = [d[IcarusInfo].srcs for d in ctx.attr.deps])
    icm_deps = depset(transitive = [d[IcarusInfo].icms for d in ctx.attr.deps])
    mod_deps = depset(transitive = [d[IcarusInfo].mods for d in ctx.attr.deps])
    lbl_deps = depset(transitive = [d[IcarusInfo].lbls for d in ctx.attr.deps])

    icm_file = ctx.actions.declare_file("{label}.icm".format(
        label = ctx.label.name
    ))


    mod_file = ctx.actions.declare_file(ctx.label.name + ".icmod")
    _compile_one(ctx, icm_file, icm_deps, mod_file, False)

    info = IcarusInfo(
            srcs = depset(transitive = ([src_deps] +
                                        [src.files for src in ctx.attr.srcs])),
            icms = depset([icm_file], transitive = [icm_deps]),
            mods = depset([mod_file], transitive = [mod_deps]),
            lbls = depset([(ctx.label, icm_file)], transitive = [lbl_deps]),
        )
    return [
        info,
        DefaultInfo(files = depset([icm_file, mod_file])),
    ]


ic_library = rule(
    implementation = _ic_library_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = [".ic"]),
        "deps": attr.label_list(providers = [IcarusInfo]),
        "_compile": attr.label(
            default = Label("//toolchain:compile"),
            allow_single_file = True,
            executable = True,
            cfg = ic_tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),

    },
)


def _ic_binary_impl(ctx):
    src_deps = depset(transitive = [d[IcarusInfo].srcs for d in ctx.attr.deps])
    icm_deps = depset(transitive = [d[IcarusInfo].icms for d in ctx.attr.deps])
    mod_deps = depset(transitive = [d[IcarusInfo].mods for d in ctx.attr.deps])
    lbl_deps = depset(transitive = [d[IcarusInfo].lbls for d in ctx.attr.deps])

    icm_file = ctx.actions.declare_file("{label}.icm".format(
        label = ctx.label.name
    ))

    mod_file = ctx.actions.declare_file(ctx.label.name + ".icmod")
    _compile_one(ctx, icm_file, icm_deps, mod_file, False)

    run_mod_file = ctx.actions.declare_file(ctx.label.name + ".run_icmod")
    _module_map_file(ctx, run_mod_file, True)

    runfiles = ctx.runfiles(
        files = ([ctx.executable._run_bytecode, icm_file, run_mod_file] +
                 icm_deps.to_list())
    )
    ctx.actions.write(
        output = ctx.outputs.executable,
        is_executable = True,
       content = """
        {executable} --input={icm} --module_map_file={mod}
        """.format(
            executable = ctx.executable._run_bytecode.short_path,
            icm = icm_file.short_path,
            mod = run_mod_file.short_path,
        )
    )

    return [
        DefaultInfo(
            files = depset([icm_file, mod_file], transitive = [icm_deps]),
            runfiles = runfiles
        ),
    ]


ic_binary = rule(
    implementation = _ic_binary_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = [".ic"]),
        "deps": attr.label_list(providers = [IcarusInfo]),
        "_compile": attr.label(
            default = Label("//toolchain:compile"),
            allow_single_file = True,
            executable = True,
            cfg = ic_tooling_transition,
        ),
        "_run_bytecode": attr.label(
            default = Label("//toolchain:run_bytecode"),
            allow_single_file = True,
            executable = True,
            cfg = ic_tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
    executable = True,
)
