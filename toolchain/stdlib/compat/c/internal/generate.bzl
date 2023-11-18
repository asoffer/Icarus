load("//toolchain:ic_rule.bzl", "ic_library")

def generated_ic_library(name, **kwargs):
    if len(kwargs["srcs"]) != 1:
        fail("Must provide exactly one source template.")
        return

    native.genrule(
        name = "internal_generate_c_values_{}_src".format(name),
        outs = ["internal_generate_c_values_{}_src.cc".format(name)],
        cmd = "./$(location //toolchain/stdlib/compat/c/internal:generate_c_values) \"{}\" {} > $@".format(
           "".join(["#include <{}>\n".format(i) for i in kwargs["includes"]]),
           " ".join(["{} \"{}\"".format(*kv) for kv in kwargs["symbols"].items()])
        ),
        tools = ["//toolchain/stdlib/compat/c/internal:generate_c_values"],
        visibility = ["//visibility:private"],
    )

    native.cc_binary(
        name = "internal_generate_c_values_{}_bin".format(name),
        srcs = [":internal_generate_c_values_{}_src".format(name)],
        visibility = ["//visibility:private"],
    )

    native.genrule(
        name = "internal_generate_c_values_{}_gen".format(name),
        outs = ["internal_generate_c_values_{}".format(name)],
        cmd = "./$(location //toolchain/stdlib/compat/c:internal_generate_c_values_{}_bin) > $@".format(
            name
        ),
        tools = ["//toolchain/stdlib/compat/c:internal_generate_c_values_{}_bin".format(name)],
        visibility = ["//visibility:private"],
    )

    native.genrule(
        name = "internal_generate_{}".format(name),
        outs = ["{}.ic".format(name)],
        srcs = kwargs["srcs"] + [":internal_generate_c_values_{}_gen".format(name)],
        cmd = "./$(location //toolchain/stdlib/compat/c/internal:generate) $(location {}) $(location {}) > $@".format(
            kwargs["srcs"][0],
            ":internal_generate_c_values_{}_gen".format(name),
        ),
        tools = ["//toolchain/stdlib/compat/c/internal:generate"],
        visibility = ["//visibility:private"],
    )

    ic_library(
        name = name,
        srcs = [":internal_generate_{}".format(name)],
        deps = kwargs["deps"],
    )
