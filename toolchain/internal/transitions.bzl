_TRANSITION_KIND = "dbg"

def _tooling_transition_impl(settings, attr):
    if _TRANSITION_KIND == "dbg":
        return {}
    else:
        return {
            "//command_line_option:compilation_mode": "opt",
            "//command_line_option:cpu": "clang",
        }


ic_tooling_transition = transition(
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
