package(default_visibility = ["//visibility:public"])

cc_library(
    name = "arithmetic",
    hdrs = ["arithmetic.h"],
    deps = [
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
    ],
)

cc_library(
    name = "base",
    hdrs = ["base.h"],
    deps = [
        "//base:cast",
        "//base:meta",
        "//base:serialize",
        "//base:traverse",
        "//base:untyped_buffer",
        "//ir/byte_code:writer",
        "//ir/instruction:inliner",
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
    deps = ["//base:extend"],
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
    deps = [
        "//base:meta",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//ir/value:result_buffer",
    ]
)

cc_test(
    name = "inliner_test",
    srcs = ["inliner_test.cc"],
    deps = [
        ":inliner",
        "//base:traverse",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "instruction",
    hdrs = ["instructions.h"],
    deps = [
        ":base",
        ":debug",
        "//ast:scope",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
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
        "//ir/value:arguments",
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
    srcs = ["core.cc"],
    deps = [
        ":debug",
        "//base:extend",
        "//base/extend:traverse",
        "//ir/blocks:basic",
        "//ir/interpreter:architecture",
        "//ir/byte_code:reader",
        "//ir/byte_code:writer",
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
