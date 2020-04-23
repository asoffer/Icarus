package(default_visibility = ["//visibility:public"])

cc_library(
    name = "base",
    hdrs = ["base.h"],
    deps = [
        "//base:cast",
        "//base:clone",
        "//base:untyped_buffer",
    ]
)

cc_library(
    name = "jump",
    hdrs = ["jump.h"],
    deps = [
        "//base:meta",
        "//base:stringify",
        "//core:fn_args",
        "//ir/value:reg",
        "//ir:results",
        "//type:typed_value",
    ],
)

cc_library(
    name = "op_codes",
    hdrs = ["op_codes.h"],
)

cc_library(
    name = "inliner",
    hdrs = ["inliner.h"],
    srcs = ["inliner.cc"],
    deps = [
        ":base",
        ":jump",
        "//base:debug",
        "//ir:local_block_interpretation",
        "//ir:results",
        "//ir/blocks:basic",
        "//ir/blocks:group",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
    ]
)

cc_test(
    name = "inliner_test",
    srcs = ["inliner_test.cc"],
    deps = [
        ":inliner",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:jump",
        "//type",
        "//type:function",
        "//type:jump",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "instruction",
    hdrs = ["instructions.h", "util.h"],
    deps = [
        ":base",
        ":inliner",
        ":op_codes",
        "//ast/scope",
        "//base:clone",
        "//ir:block_def",
        "//ir:out_params",
        "//ir:scope_def",
        "//ir:struct_field",
        "//ir/value:fn",
        "//ir/value:generic_fn",
        "//ir/value:reg_or",
        "//ir/value:string",
        "//module",
        "//type",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:generic_function",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
        "//type:util",
        "//type:variant",
        "@com_google_absl//absl/strings",
    ],
)
