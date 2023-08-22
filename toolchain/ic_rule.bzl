load("//toolchain/internal:transitions.bzl", "ic_tooling_transition")

IcarusInfo = provider(
    "Information needed to compile and link an Icarus binary",
    fields = ["srcs"],
)


def _ic_binary_impl(ctx):
    if len(ctx.attr.srcs) != 1:
        fail("library rules must have exactly one file in 'srcs'.")
    src = ctx.attr.srcs[0]
    src_file = src.files.to_list()[0]

    icm_file = ctx.actions.declare_file("{label}.icm".format(
        label = ctx.label.name
    ))

    ctx.actions.run(
        inputs = depset([src_file]),
        outputs = [icm_file],
        arguments = [
            src_file.short_path,
            "--output={}".format(icm_file.path),
        ],
        progress_message = "Compiling {}".format(ctx.label.name),
        executable = ctx.attr._compile[0][DefaultInfo].files_to_run.executable,
    )

    runfiles = ctx.runfiles(
        files = [icm_file]
    )

    ctx.actions.write(
        output = ctx.outputs.executable,
        is_executable = True,
        content = """
        """
    )
    return [
        DefaultInfo(
            files = depset([icm_file]),
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
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
    executable = True,
)
