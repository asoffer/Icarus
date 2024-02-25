load("//toolchain/bazel:provider.bzl", "IcarusInfo")
load("//toolchain/bazel:transitions.bzl", "ic_tooling_transition")

def _ic_builtin_library_impl(ctx):
    icm_file = ctx.actions.declare_file("{label}.icm".format(
        label = ctx.label.name
    ))

    ctx.actions.run(
        inputs = depset(),
        outputs = [icm_file],
        arguments = [
            icm_file.path,
        ],
        progress_message = "Compiling //{}:{}".format(ctx.label.package, 
                                                      ctx.label.name),
        executable = ctx.attr._construct[0][DefaultInfo].files_to_run.executable,
    )

    return [
        DefaultInfo(
            files = depset([icm_file]),
        ),
        IcarusInfo(
            deps = [],
            icm = icm_file,
            data_deps = depset(),
        ),
    ]


ic_builtin_library = rule(
    implementation = _ic_builtin_library_impl,
    attrs = {
        "_construct": attr.label(
            default = Label("//toolchain/builtin:construct"),
            allow_single_file = True,
            executable = True,
            cfg = ic_tooling_transition,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),
    },
)
