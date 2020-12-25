package(default_visibility = ["//compiler:__subpackages__"])

cc_library(
    name = "access",
    srcs = ["access.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler:library_module",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type:enum",
        "//type:flags",
        "//type:typed_value",
    ],
)

cc_test(
    name = "access_test",
    srcs = ["access_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
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
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "array_type",
    srcs = ["array_type.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "array_type_test",
    srcs = ["array_type_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "assignment",
    srcs = ["assignment.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//ir/value",
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
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)


cc_library(
    name = "binary_operator",
    srcs = ["binary_operator.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "binary_operator_test",
    srcs = ["binary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "block_literal",
    srcs = ["block_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//ir/value",
    ],
)

cc_test(
    name = "block_literal_test",
    srcs = ["block_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "block_node",
    srcs = ["block_node.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//ir:builder",
        "//ir/value",
    ],
)

cc_library(
    name = "call",
    srcs = ["call.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//compiler:resources",
        "//ir/value",
    ],
)

cc_test(
    name = "call_test",
    srcs = ["call_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "cast",
    srcs = ["cast.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "cast_test",
    srcs = ["cast_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "comparison_operator",
    srcs = ["comparison_operator.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "comparison_operator_test",
    srcs = ["comparison_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "common",
    hdrs = ["common.h"],
    srcs = ["common.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//core:arguments",
        "//core:params",
        "//ir/value",
        "//type",
        "//type:qual_type",
        "//type:typed_value",
    ],
)

cc_library(
    name = "declaration",
    srcs = ["declaration.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//ir/value",
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
        "//ast",
        "//compiler:compiler_header",
        "//ir:builder",
        "//ir/value",
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
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "enum_literal",
    srcs = ["enum_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

# TODO: Enum literal tests

cc_library(
    name = "function_type",
    srcs = ["function_type.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//ir:builder",
        "//ir/value",
        "//ir/value:reg_or",
        "//type",
    ],
)

# TODO: FunctionType

cc_library(
    name = "function_literal",
    srcs = ["function_literal.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//base:move_func",
        "//core:arguments",
        "//ir/value",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
    ],
)

# TODO: FunctionLiteral

cc_library(
    name = "goto",
    srcs = ["goto.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

# Note: There are no tests for ":goto" directly as these are well-covered by
# ":scope_node_test".

cc_library(
    name = "identifier",
    srcs = ["identifier.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "identifier_test",
    srcs = ["identifier_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "index",
    srcs = ["index.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "index_test",
    srcs = ["index_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "jump",
    srcs = ["jump.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//base:move_func",
        "//core:arguments",
        "//ir/value",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
    ],
)

# TODO: Jump 

cc_library(
    name = "miscellaneous",
    srcs = ["miscellaneous.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//ir/value",
    ],
)

# TODO: Miscellaneous test

cc_library(
    name = "parameterized_struct_literal",
    srcs = ["parameterized_struct_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler/emit:common",
        "//ir/value",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

# TODO: ParameterizedStructLiteral tests.

cc_library(
    name = "return_stmt",
    srcs = ["return_stmt.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler/emit:common",
        "//ir/value",
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
        "//ast",
        "//compiler:compiler_header",
        "//ir/value",
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
        "//ast",
        "//base:defer",
        "//compiler:compiler_header",
        "//ir:compiled_scope",
        "//ir/instruction:inliner",
        "//ir/value:scope",
        "@com_google_absl//absl/strings:str_format",
    ],
)

cc_test(
    name = "scope_node_test",
    srcs = ["scope_node_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "short_function_literal",
    srcs = ["short_function_literal.cc"],
    deps = [
        ":common",
        "//ast",
        "//compiler:compiler_header",
        "//base:move_func",
        "//core:arguments",
        "//ir/value",
        "//ir/value:reg",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
    ],
)

# TODO: ShortFunctionLiteral

cc_library(
    name = "special",
    srcs = ["special.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "special_test",
    srcs = ["special_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "struct_literal",
    srcs = ["struct_literal.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler/emit:common",
        "//ir/value",
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
        "//ast",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "terminal_test",
    srcs = ["terminal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "unary_operator",
    srcs = ["unary_operator.cc"],
    deps = [
        "//ast",
        "//base:defer",
        "//frontend/lex:operators",
        "//compiler:compiler_header",
    ],
)

cc_test(
    name = "unary_operator_test",
    srcs = ["unary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "yield_stmt",
    srcs = ["yield_stmt.cc"],
    deps = [
        "//ast",
        "//compiler:compiler_header",
        "//compiler/emit:common",
        "//ir/value",
        "//ir/value:addr",
        "//ir/value:reg_or",
        "//type",
        "//type:typed_value",
        "@com_google_absl//absl/types:span",
    ],
)

# TODO: yieldStmt tests.
