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
        ":serializer",
        "//base:cast",
        "//base:meta",
        "//base:serialize",
        "//base:traverse",
        "//base:untyped_buffer",
        "//ir/byte_code:writer",
        "//ir/instruction:inliner",
        "//ir/interpreter:stack_frame",
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
        "//base:extend",
        "//ir/value:reg",
        "//ir/value:reg_or",
    ],
)

cc_library(
    name = "debug",
    hdrs = ["debug.h"],
    deps = [
        "//base:extend",
        "//base:universal_print",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_library(
    name = "foreign",
    hdrs = ["foreign.h"],
    srcs = ["foreign.cc"],
    visibility = ["//visibility:public"],
    deps = [
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
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
        ":foreign",
        "//ast:scope",
        "//base:extend",
        "//base/extend:serialize",
        "//base/extend:traverse",
        "//ir:subroutine",
        "//ir/interpreter",
        "//ir:out_params",
        "//ir/value:fn",
        "//ir/value:generic_fn",
        "//ir/value:reg_or",
        "//module",
        "//type:array",
        "//type:generic",
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
        "//base:universal_print",
        "//core:arguments",
        "//ir/value:block",
        "//ir/value:reg",
        "//type:qual_type",
    ],
)

cc_library(
    name = "core",
    hdrs = ["core.h"],
    srcs = ["core.cc"],
    deps = [
        ":debug",
        "//base:extend",
        "//base/extend:traverse",
        "//ir:basic_block",
        "//ir/byte_code:reader",
        "//ir/byte_code:writer",
        "//ir/interpreter",
        "//ir:out_params",
        "//ir/value:reg_or",
        "//ir/value:slice",
        "//type:function",
        "@com_google_absl//absl/strings",
    ],
)

proto_library(
    name = "instruction_proto",
    srcs = ["instruction.proto"],
)

cc_proto_library(
    name = "instruction_cc_proto",
    deps = [":instruction_proto"],
)

cc_library(
    name = "serializer",
    hdrs = ["serializer.h"],
    deps = [
        ":instruction_cc_proto",
        "//base:flyweight_set",
        "//base:meta",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/types:span",
    ],
)
