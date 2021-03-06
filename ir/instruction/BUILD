package(default_visibility = ["//visibility:public"])

cc_library(
    name = "arithmetic",
    hdrs = ["arithmetic.h"],
    deps = [
        ":inliner",
        "//base:extend",
    ],
)

cc_library(
    name = "base",
    hdrs = ["base.h"],
    deps = [
        "//base:cast",
        "//base:meta",
        "//base:untyped_buffer",
        "//ir/value:reg",
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
    name = "compare",
    hdrs = ["compare.h"],
    deps = [
        ":inliner",
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
    name = "inliner",
    hdrs = ["inliner.h"],
    srcs = ["inliner.cc"],
    deps = [
        ":base",
        "//base:debug",
        "//base:meta",
        "//ir:compiled_jump",
        "//ir:local_block_interpretation",
        "//ir/blocks:basic",
        "//ir/blocks:group",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type:typed_value",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/strings:str_format",
    ]
)

cc_test(
    name = "inliner_test",
    srcs = ["inliner_test.cc"],
    deps = [
        ":arithmetic",
        ":inliner",
        "//ir:builder",
        "//ir:compiled_fn",
        "//ir:compiled_jump",
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
        ":base",
        ":debug",
        ":inliner",
        "//ast:scope",
        "//base:extend",
        "//ir:compiled_block",
        "//ir:compiled_scope",
        "//ir/interpreter:execution_context",
        "//ir:out_params",
        "//ir/value:block",
        "//ir/value:fn",
        "//ir/value:generic_fn",
        "//ir/value:reg_or",
        "//ir/value:string",
        "//module",
        "//type:array",
        "//type:generic_function",
        "//type:pointer",
        "//type:struct",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "jump",
    hdrs = ["jump.h"],
    deps = [
        "//base:meta",
        "//base:stringify",
        "//core:arguments",
        "//ir/value",
        "//ir/value:reg",
        "//type:qual_type",
    ],
)

cc_library(
    name = "op_codes",
    hdrs = ["op_codes.h"],
)

cc_library(
    name = "core",
    hdrs = ["core.h"],
    deps = [
        ":debug",
        "//base:extend",
        "//ir/blocks:basic",
        "//ir:out_params",
        "//ir/value:reg_or",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "set",
    hdrs = ["set.h"],
    deps = [
        ":core",
        "//base:meta",
        "//base:untyped_buffer",
        "//ir:compiled_jump",
        "//ir/interpreter:architecture",
        "//ir/interpreter:foreign",
        "//ir/interpreter:stack_frame",
        "//ir:read_only_data",
        "//ir/value:addr",
        "//ir/value:block",
        "//ir/value:char",
        "//ir/value:foreign_fn",
        "//ir/value:fn",
        "//ir/value:native_fn",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)
