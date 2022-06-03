#include "compiler/compiler.h"
#include "compiler/module.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(BuiltinReserveMemory, FunctionSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.reserve_memory(1, 1))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt,
            type::QualType::NonConstant(type::Type(type::BufPtr(type::Byte))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinReserveMemory, NonConstantArgument) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(
  n := 3 as u64
  builtin.reserve_memory(n, 1)
  )");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncallable-with-arguments")));
}

TEST(BuiltinReserveMemory, WrongType) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.reserve_memory(true, 1))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncallable-with-arguments")));
}

TEST(BuiltinCompilationError, CompilationErrorError) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(builtin.compilation_error(0, "hello"))");
  auto const *call = mod.get<ast::Call>();
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinCompilationError, CompilationErrorSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(builtin.compilation_error(i64, "hello"))");
  auto const *call = mod.get<ast::Call>();
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "user-defined-error")));
}

TEST(BuiltinForeign, FunctionSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod =
      infra.add_module(R"(builtin.foreign("my_function", i64 -> bool))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(
      qt,
      type::QualType::Constant(type::Func(
          {core::AnonymousParameter(type::QualType::NonConstant(type::I64))},
          {type::Bool})));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, PointerSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.foreign("my_ptr", *i64))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Ptr(type::I64)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, BufferPointerSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(builtin.foreign("my_array", [*]i64))");
  auto const *call = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::BufPtr(type::I64)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, NamedArgs) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(
      R"(builtin.foreign(name = "my_function", foreign_type = i64 -> bool))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, NoArgs) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module("builtin.foreign()");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, OneArg) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.foreign("abc"))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, TooManyArgs) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.foreign("abc", 1, 2, 3))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, FirstParameterCharSlice) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.foreign(123, *i64))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, NonConstantType) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(
  f := () -> ()
  builtin.foreign("f", f)
  )");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, SecondParameterType) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.foreign("f", 3))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinOpaque, Success) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module("builtin.opaque()");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinOpaque, Arguments) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module("builtin.opaque(3)");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncallable-with-arguments")));
}

TEST(BuiltinCallable, WithNoArguments) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.callable())");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Interface));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinCallable, WithOneType) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.callable(i32))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Interface));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinCallable, WithOneInterface) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.callable(builtin.callable()))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Interface));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinCallable, WithMultiple) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(
      R"(builtin.callable(builtin.callable(), i32, builtin.callable()))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Interface));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BuiltinCallable, WrongType) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module(R"(builtin.callable(3))");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(Call, Uncallable) {
  test::CompilerInfrastructure infra;
  auto &mod         = infra.add_module("3()");
  auto const *call  = mod.get<ast::Call>();
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncallable-with-arguments")));
}

TEST(Call, CrossModuleCallsWithoutADLGenerateErrors) {
  test::CompilerInfrastructure infra;
  auto &imported_mod = infra.add_module("imported", R"(
  #{export} S ::= struct {}
  #{export} f ::= (s: S) => true
  )");

  auto &mod = infra.add_module(R"(
    mod ::= import "imported"
    s: mod.S
    f(s)
  )");

  EXPECT_THAT(infra.diagnostics(), UnorderedElementsAre(Pair(
                                       "type-error", "undeclared-identifier")));
}

TEST(Call, CrossModuleWithADLSucceed) {
  test::CompilerInfrastructure infra;
  auto &imported_mod = infra.add_module("imported", R"(
  #{export} S ::= struct {}
  #{export} f ::= (s: S) => 3 as i64
  )");

  auto &mod = infra.add_module(R"(
    mod ::= import "imported"
    f ::= (n: i64) => true
    s: mod.S
    s'f
  )");
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Call, CrossModuleWithADLWithoutExport) {
  test::CompilerInfrastructure infra;
  auto &imported_mod = infra.add_module("imported", R"(
  #{export} S ::= struct {}
  f ::= (s: S) => 3 as i64
  )");
  auto &mod          = infra.add_module(R"(
    mod ::= import "imported"
    s: mod.S
    s'f
  )");

  EXPECT_THAT(infra.diagnostics(), UnorderedElementsAre(Pair(
                                       "type-error", "undeclared-identifier")));
}

struct TestCase {
  std::string context;
  std::string expr;
  type::QualType expected_qual_type = type::QualType::Error();
  testing::Matcher<absl::Span<std::pair<std::string, std::string> const>>
      expected_diagnostics = IsEmpty();
};

// TODO: This is starting to get to the point where it should be shared test
// infrastructure.
using CallTest = testing::TestWithParam<TestCase>;
TEST_P(CallTest, Call) {
  auto const &[context, expr, expected_qual_type, expected_diagnostics] =
      GetParam();
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(absl::StrCat(context, "\n", expr));
  auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
  EXPECT_THAT(qts, UnorderedElementsAre(expected_qual_type));
  EXPECT_THAT(infra.diagnostics(), expected_diagnostics);
}

INSTANTIATE_TEST_SUITE_P(
    All, CallTest,
    testing::ValuesIn({
        TestCase{
            .context            = "f ::= () => 3 as i64",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context              = "f ::= () => 3",
            .expr                 = "f(true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64) => n",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (n: i64) => n",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context              = "f ::= (n: i64) => 3",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64) => 3",
            .expr                 = "f(b = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 0) => n",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 0) => n",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 0) => n",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context              = "f ::= (n: i64 = 0) => 3",
            .expr                 = "f(b = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64 = 0) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64, b: bool) => b",
            .expr               = "f(4, true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(b = true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: i64, b: bool) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(4, true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (n: i64 = 3, b: bool = true) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(b = true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f(b = true, n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: i64 = 3, b: bool = true) => b",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        // Overload sets
        TestCase{
            .context            = R"(
            f ::= () => true
            f ::= (n: i64) => n
            )",
            .expr               = "f(0)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = R"(
            f ::= () => true
            f ::= (n: i64) => n
            )",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = R"(
            f ::= (b: bool) => true
            f ::= (n: i64) => n
            )",
            .expr               = "f(n = 0)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        // Generic functions
        TestCase{
            .context            = "f ::= (x: ~`T) => x",
            .expr               = "f(3 as i64)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (x: ~`T) => x",
            .expr               = "f(true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (x: ~`T) => x",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
    }));

}  // namespace
}  // namespace compiler
