package(default_visibility = ["//compiler:__subpackages__"])

cc_library(
    name = "access",
    srcs = ["access.cc"],
    deps = [
        ":common",
        ":copy_move_assignment",
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:module",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type:enum",
        "//type:block",
        "//type:flags",
        "//type:typed_value",
    ],
)

cc_test(
    name = "access_test",
    srcs = ["access_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "array_literal",
    srcs = ["array_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "array_literal_test",
    srcs = ["array_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "array_type",
    srcs = ["array_type.cc"],
    deps = [
        ":compiler_common", 
        ":common",
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "array_type_test",
    srcs = ["array_type_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "assignment",
    srcs = ["assignment.cc"],
    deps = [
        ":common",
        ":copy_move_assignment",
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:addr",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "assignment_test",
    srcs = ["assignment_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)


cc_library(
    name = "binary_operator",
    srcs = ["binary_operator.cc"],
    deps = [
        ":common", 
        ":compiler_common", 
        ":copy_move_assignment",
        ":initialize",
        "//ast",
        "//compiler:compiler_header", 
        "//type:cast",
    ],
)

cc_test(
    name = "binary_operator_test",
    srcs = ["binary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "//test:module",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "block_node",
    srcs = ["block_node.cc"],
    deps = [
        ":common",
        ":compiler_common",
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_library(
    name = "call",
    srcs = ["call.cc"],
    deps = [
        ":common",
        ":compiler_common",
        ":copy_move_assignment",
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:instantiate",
        "//compiler:module",
        "//compiler:resources",
        "//ir/instruction",
        "//type:generic",
    ],
)

cc_test(
    name = "call_test",
    srcs = ["call_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "cast",
    srcs = ["cast.cc"],
    deps = [
        ":common",
        ":compiler_common",
        ":copy_move_assignment",
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:char",
    ],
)

cc_test(
    name = "cast_test",
    srcs = ["cast_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "comparison_operator",
    srcs = ["comparison_operator.cc"],
    deps = [
        ":common",
        ":compiler_common",
        ":copy_move_assignment",
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:char",
        "//type:cast",
    ],
)

cc_test(
    name = "comparison_operator_test",
    srcs = ["comparison_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "common",
    hdrs = ["common.h"],
    srcs = ["common.cc"],
    deps = [
        "//compiler:compilation_data",
        "//compiler:instructions",
        "//compiler:transient_state",
        "//core:arguments",
        "//core:parameters",
        "//ir/instruction",
        "//ir/instruction:compare",
        "//ir/value:result_buffer",
        "//type",
        "//type:cast",
        "//type:enum",
        "//type:flags",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_library(
    name = "compiler_common",
    hdrs = ["compiler_common.h"],
    srcs = ["compiler_common.cc"],
    deps = [
        ":common",
        ":copy_move_assignment",
        "//ast",
        "//compiler:common",
        "//compiler:compiler_header",
        "//compiler:instantiate",
        "//compiler:instructions",
        "//compiler:module",
        "//core:arguments",
        "//core:parameters",
        "//ir/value:result_buffer",
        "//type",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_library(
    name = "declaration",
    srcs = ["declaration.cc"],
    deps = [
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:module",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

# TODO: Declaration tests.

cc_library(
    name = "designated_initializer",
    srcs = ["designated_initializer.cc"],
    deps = [
        ":copy_move_assignment",
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "designated_initializer_test",
    srcs = ["designated_initializer_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "enum_literal",
    srcs = ["enum_literal.cc"],
    deps = [
        ":common",
        ":compiler_common",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:module",
    ],
)

# TODO: Enum literal tests

cc_library(
    name = "function_type",
    srcs = ["function_type.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:reg_or",
        "//type",
        "//type:function_instructions",
    ],
)

cc_test(
    name = "function_type_test",
    srcs = ["function_type_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "function_literal",
    srcs = ["function_literal.cc"],
    deps = [
        ":common",
        ":compiler_common",
        ":copy_move_assignment",
        ":initialize",
        ":scaffolding",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:instantiate",
        "//compiler:instructions",
        "//compiler:resources",
        "//core:arguments",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
    ],
)

# TODO: function_literal_test

cc_library(
    name = "identifier",
    srcs = ["identifier.cc"],
    deps = [
        ":common",
        ":compiler_common",
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:module",
    ],
)

cc_test(
    name = "identifier_test",
    srcs = ["identifier_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "index",
    srcs = ["index.cc"],
    deps = [
        ":common",
        ":compiler_common",
        ":copy_move_assignment",
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "index_test",
    srcs = ["index_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "miscellaneous",
    srcs = ["miscellaneous.cc"],
    deps = [
        ":common",
        ":compiler_common",
        "//ast",
        "//compiler:compiler_header",
    ],
)

# TODO: Miscellaneous test

cc_library(
    name = "parameterized_struct_literal",
    srcs = ["parameterized_struct_literal.cc"],
    deps = [
        ":common",
        ":struct_literal",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:instructions",
        "//compiler:struct",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

# TODO: ParameterizedStructLiteral tests.

cc_library(
    name = "pattern_match",
    srcs = ["pattern_match.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type",
    ],
)

cc_test(
    name = "pattern_match_test",
    srcs = ["pattern_match_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "return_stmt",
    srcs = ["return_stmt.cc"],
    deps = [
        ":common",
        ":compiler_common",
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

# TODO: ReturnStmt tests.

cc_library(
    name = "scope_literal",
    srcs = ["scope_literal.cc"],
    deps = [
        ":common",
        ":compiler_common",
        ":scaffolding",
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:reg_or",
        "//type",
    ],
)

# TODO: ScopeLiteral test

cc_library(
    name = "scope_node",
    srcs = ["scope_node.cc"],
    deps = [
        ":common",
        ":compiler_common",
        "//ast",
        "//compiler:common",
        "//compiler:compiler_header",
        "//compiler:instructions",
        "//ir:subroutine",
        "//ir/value:scope",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/strings:str_format",
    ],
)

# TODO: ScopeNode test

cc_library(
    name = "short_function_literal",
    srcs = ["short_function_literal.cc"],
    deps = [
        ":common",
        ":compiler_common",
        ":scaffolding",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:instantiate",
        "//compiler:instructions",
        "//compiler:resources",
        "//core:arguments",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
    ],
)

# TODO: ShortFunctionLiteral

cc_library(
    name = "slice_type",
    srcs = ["slice_type.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//type:slice",
    ],
)

cc_test(
    name = "slice_type_test",
    srcs = ["slice_type_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "copy_move_assignment",
    hdrs = ["copy_move_assignment.h"],
    srcs = ["copy_move_assignment.cc"],
    deps = [
        ":common",
        "//compiler:compilation_data",
        "//compiler:instructions",
        "//type:cast",
        "//ir/value:addr",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "//type:slice",
        "//type:scope",
        "//type:typed_value",
    ],
)

# TODO: Figure out what to do with this.
cc_test(
    name = "special_test",
    srcs = ["special_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "struct_literal",
    hdrs = ["struct_literal.h"],
    srcs = ["struct_literal.cc"],
    deps = [
        ":common",
        ":copy_move_assignment",
        ":destroy",
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:instructions",
        "//compiler:module",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

# TODO: StructLiteral tests.

cc_library(
    name = "terminal",
    srcs = ["terminal.cc"],
    deps = [
        ":copy_move_assignment",
        ":initialize",
        "//ast",
        "//compiler:compiler_header",
        "//type:primitive",
        "//type:slice",
    ],
)

cc_test(
    name = "terminal_test",
    srcs = ["terminal_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "unary_operator",
    srcs = ["unary_operator.cc"],
    deps = [
        ":common",
        ":compiler_common", 
        ":copy_move_assignment",
        ":destroy",
        ":initialize",
        "//ast",
        "//frontend/lex:operators",
        "//compiler:compiler_header",
        "//type:pointer",
        "//type:pointer_instructions",
        "//type:struct",
    ],
)

cc_test(
    name = "unary_operator_test",
    srcs = ["unary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:evaluation",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "yield_stmt",
    srcs = ["yield_stmt.cc"],
    deps = [
        ":common",
        ":compiler_common",
        "//ast",
        "//compiler:compiler_header",
        "//ir/value:addr",
        "//ir/value:char",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

# TODO: YieldStmt tests.

cc_library(
    name = "initialize",
    hdrs = ["initialize.h"],
    srcs = ["initialize.cc"],
    deps = [
        ":common",
        "//compiler:compilation_data",
        "//compiler:instructions",
        "//type:cast",
        "//ir/value:addr",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "//type:slice",
        "//type:scope",
        "//type:typed_value",
    ],
)

cc_library(
    name = "destroy",
    hdrs = ["destroy.h"],
    deps = [
        ":common",
        "//compiler:compilation_data",
        "//compiler:instructions",
        "//type:cast",
        "//ir/value:addr",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:array",
        "//type:enum",
        "//type:flags",
        "//type:function",
        "//type:pointer",
        "//type:primitive",
        "//type:slice",
        "//type:scope",
    ],
)

cc_library(
    name = "scaffolding",
    hdrs = ["scaffolding.h"],
    srcs = ["scaffolding.cc"],
    deps = [
        ":destroy",
        "//ast:scope",
        "//base:debug",
        "//compiler:compilation_data",
        "//compiler:transient_state",
        "//ir:subroutine",
    ]
)
