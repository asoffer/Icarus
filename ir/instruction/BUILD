package(default_visibility = ["//visibility:public"])

cc_library(
    name = "arithmetic",
    hdrs = ["arithmetic.h"],
    deps = [
        ":inliner",
        ":op_codes",
        ":util",
        "//base:extend",
        "//ir/interpretter:execute",
    ],
)

cc_library(
    name = "base",
    hdrs = ["base.h"],
    deps = [
        "//base:cast",
        "//base:meta",
        "//base:untyped_buffer",
    ]
)

cc_test(
    name = "base_test",
    srcs = ["base_test.cc"],
    deps = [
        ":base",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "byte_view",
    hdrs = ["byte_view.h"],
    deps = [
        ":inliner",
        ":op_codes",
        "//base:extend",
    ],
)

cc_library(
    name = "compare",
    hdrs = ["compare.h"],
    deps = [
        ":inliner",
        ":op_codes",
        ":util",
        "//base:extend",
    ],
)

cc_library(
    name = "debug",
    hdrs = ["debug.h"],
    deps = [
        "//base:extend",
        "//base:stringify",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "flags",
    hdrs = ["flags.h"],
    deps = [
        ":inliner",
        ":op_codes",
        ":util",
        "//base:extend",
    ],
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
    hdrs = ["instructions.h"],
    deps = [
        ":arithmetic",
        ":base",
        ":compare",
        ":byte_view",
        ":debug",
        ":flags",
        ":inliner",
        ":op_codes",
        ":phi",
        ":util",
        ":type",
        "//ast/scope",
        "//base:extend",
        "//ir:block_def",
        "//ir:out_params",
        "//ir:scope_def",
        "//ir/value:fn",
        "//ir/value:generic_fn",
        "//ir/value:reg_or",
        "//ir/value:string",
        "//module",
        "//type:util",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "jump",
    hdrs = ["jump.h"],
    deps = [
        "//base:meta",
        "//base:stringify",
        "//core:fn_args",
        "//ir/value",
        "//ir/value:reg",
        "//type:typed_value",
    ],
)

cc_library(
    name = "op_codes",
    hdrs = ["op_codes.h"],
)

cc_library(
    name = "phi",
    hdrs = ["phi.h"],
    deps = [
        ":op_codes",
        "//base:extend",
        "//ir/blocks:basic",
        "//ir/interpretter:execute",
        "//ir/value:reg_or",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "type",
    hdrs = ["type.h"],
    deps = [
        ":inliner",
        ":op_codes",
        ":util",
        "//base:extend",
        "//ir:struct_field",
        "//type",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:generic_function",
        "//type:opaque",
        "//type:pointer",
        "//type:struct",
        "//type:tuple",
    ],
)

# TODO: Remove this target.
cc_library(
    name = "util",
    hdrs = ["util.h"],
    deps = [
        "//ir:block_def",
        "//ir:out_params",
        "//ir:scope_def",
        "//ir/value:fn",
        "//ir/value:generic_fn",
        "//ir/value:reg_or",
        "//ir/value:string",
        "//module",
        "//type",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "set",
    hdrs = ["set.h"],
    deps = [
        "//base:meta",
        "//base:untyped_buffer",
        "//ir:block_def",
        "//ir/interpretter:architecture",
        "//ir/interpretter:execute",
        "//ir/interpretter:foreign",
        "//ir/interpretter:stack_frame",
        "//ir:jump",
        "//ir:read_only_data",
        "//ir:scope_def",
        "//ir/instruction",
        "//ir/value:addr",
        "//ir/value:foreign_fn",
        "//ir/value:fn",
        "//ir/value:native_fn",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/types:span",
    ],
)
