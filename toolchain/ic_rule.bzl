load("//toolchain/internal:transitions.bzl", "ic_tooling_transition")

IcarusInfo = provider(
    "Information needed to compile and link an Icarus binary",
    fields = ["icm", "icm_deps"],
)

def _module_name(p):
    s = str(p.label)
    name = s[s.rfind("//") + 2:].replace("/", ".").replace(":", ".")
    if name.startswith("toolchain.stdlib"):
        name = "std" + name[16:]
    return name


def _module_location(p):
    return p[IcarusInfo].icm.path

def _short_module_location(p):
    return p[IcarusInfo].icm.short_path

def _ic_compile_impl(ctx):
    src = ctx.attr.srcs[0]
    src_file = src.files.to_list()[0]

    mod_file = ctx.actions.declare_file("{label}.icmod".format(
        label = ctx.label.name
    ))

    icm_file = ctx.actions.declare_file("{label}.icm".format(
        label = ctx.label.name
    ))

    ctx.actions.write(
        output = mod_file,
        content = "\n".join([
            "{}\t{}\n".format(_module_name(p), _module_location(p))
            for p in depset(ctx.attr.deps).to_list()
        ])
    )

    icm_deps = [d[IcarusInfo].icm_deps for d in ctx.attr.deps]

    ctx.actions.run(
        inputs = depset([src_file, mod_file], transitive = icm_deps),
        outputs = [icm_file],
        arguments = [
            src_file.path,
            "--output={}".format(icm_file.path),
            "--module-map={}".format(mod_file.path),
            # "--debug-parser=true",
            # "--debug-type-check=true",
            # "--debug-emit=true",
        ],
        progress_message = "Compiling //{}:{}".format(ctx.label.package, 
                                                      ctx.label.name),
        executable = ctx.attr._compile[0][DefaultInfo].files_to_run.executable,
    )

    return icm_file, mod_file, depset([icm_file], transitive = icm_deps)

def _ic_library_impl(ctx):
    if len(ctx.attr.srcs) != 1:
        fail("ic_library rules must have exactly one file in 'srcs'.")

    (icm_file, mod_file, icm_deps) = _ic_compile_impl(ctx)

    return [
        DefaultInfo(
            files = depset([icm_file]),
        ),
        IcarusInfo(
            icm = icm_file,
            icm_deps = icm_deps,
        ),
    ]



def _ic_binary_impl(ctx):
    if len(ctx.attr.srcs) != 1:
        fail("ic_binary rules must have exactly one file in 'srcs'.")

    (icm_file, _, icm_deps) = _ic_compile_impl(ctx)

    mod_file = ctx.actions.declare_file("{label}.icrunmod".format(
        label = ctx.label.name
    ))

    ctx.actions.write(
        output = mod_file,
        content = "\n".join([
            "{}\t{}\n".format(_module_name(p), _short_module_location(p))
            for p in depset(ctx.attr.deps).to_list()
        ])
    )

    runfiles = ctx.runfiles(
        files = [ctx.executable._run_bytecode, mod_file],
        transitive_files = icm_deps,
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        is_executable = True,
        content = """
        {executable} --input={icm} --module-map={mm} $@
        """.format(
            executable = ctx.executable._run_bytecode.short_path,
            icm = icm_file.short_path,
            mm = mod_file.short_path,
        )
    )
    return [
        DefaultInfo(
            files = depset([icm_file], transitive = [icm_deps]),
            runfiles = runfiles
        ),
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
