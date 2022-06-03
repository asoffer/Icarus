package(default_visibility = ["//compiler:__subpackages__"])

cc_library(
    name = "verify",
    hdrs = ["verify.h"],
    srcs = [
        "access.cc",
        "array_literal.cc",
        "array_type.cc",
        "assignment.cc",
        "binary_operator.cc",
        "block_node.cc",
        "call.cc",
        "cast.cc",
        "common.h",
        "common.cc",
        "comparison_operator.cc",
        "declaration.cc",
        "designated_initializer.cc",
        "enum_literal.cc",
        "function_literal.cc",
        "function_type.cc",
        "identifier.cc",
        "import.cc",
        "index.cc",
        "interface_literal.cc",
        "module.cc",
        "miscellaneous.cc",
        "parameterized_struct_literal.cc",
        "pattern_match.cc",
        "program_arguments.cc",
        "scope_literal.cc",
        "slice_type.cc",
        "short_function_literal.cc",
        "struct_literal.cc",
        "terminal.cc",
        "verify.cc",
        "unary_operator.cc",
    ],
    deps = [
        "//ast",
        "//base:meta",
        "//compiler:common",
        "//compiler:common_diagnostics",
        "//compiler:compilation_data",
        "//compiler:context",
        "//compiler:instantiate",
        "//compiler:instructions",
        "//compiler:module",
        "//compiler:resources",
        "//compiler:struct",
        "//compiler:type_for_diagnostic",
        "//compiler:work_item",
        "//core:arguments",
        "//core:cycle_tracker",
        "//core:dependency_node",
        "//ir/value:module_id",
        "//type:array",
        "//type:block",
        "//type:callable",
        "//type:cast",
        "//type:enum",
        "//type:flags",
        "//type:generic",
        "//type:overload_set",
        "//type:pointer",
        "//type:primitive",
        "//type:provenance",
        "//type:qual_type",
        "//type:slice",
        "//type:struct",
        "//type:typed_value",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "access_test",
    srcs = ["access_test.cc"],
    deps = [
        "//compiler",
        "//compiler:module",
        "//test:module",
        "@com_google_googletest//:gtest_main",
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

cc_test(
    name = "array_type_test",
    srcs = ["array_type_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
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

cc_test(
    name = "binary_operator_test",
    srcs = ["binary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "//type:primitive",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "call_test",
    srcs = ["call_test.cc"],
    deps = [
        "//compiler",
        "//compiler:module",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "cast_test",
    srcs = ["cast_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
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

cc_test(
    name = "declaration_test",
    srcs = ["declaration_test.cc"],
    deps = [
        "//compiler",
        "//test:module",

        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "designated_initializer_test",
    srcs = ["designated_initializer_test.cc"],
    deps = [
        "//compiler",
        "//compiler:module",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "enum_literal_test",
    srcs = ["enum_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "function_literal_test",
    srcs = ["function_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "function_type_test",
    srcs = ["function_type_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
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

cc_test(
    name = "import_test",
    srcs = ["import_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
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

cc_test(
    name = "interface_literal_test",
    srcs = ["interface_literal_test.cc"],
    deps = [
        "//compiler",
        "//compiler:module",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

# TODO: miscellaneous_test
# TODO: parameterized_struct_literal_test

cc_test(
    name = "pattern_match_test",
    srcs = ["pattern_match_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "program_arguments_test",
    srcs = ["program_arguments_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "slice_type_test",
    srcs = ["slice_type_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "short_function_literal_test",
    srcs = ["short_function_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "struct_literal_test",
    srcs = ["struct_literal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "terminal_test",
    srcs = ["terminal_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "//type:primitive",
        "//type:qual_type",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_test(
    name = "unary_operator_test",
    srcs = ["unary_operator_test.cc"],
    deps = [
        "//compiler",
        "//test:module",
        "//type:array",
        "//type:primitive",
        "@com_google_googletest//:gtest_main",
    ],
)

