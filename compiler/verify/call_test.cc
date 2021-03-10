#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::Eq;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(BuiltinForeign, FunctionSuccess) {
  test::TestModule mod;
  auto const *call =
      mod.Append<ast::Call>(R"(foreign("my_function", i64 -> bool))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt,
            type::QualType::Constant(type::Func(
                {core::AnonymousParam(type::QualType::NonConstant(type::I64))},
                {type::Bool})));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, PointerSuccess) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("my_ptr", *i64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Ptr(type::I64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, BufferPointerSuccess) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("my_array", [*]i64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::BufPtr(type::I64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, NamedArgs) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(
      foreign(name = "my_function", foreign_type = i64 -> bool)
      )");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, NoArgs) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("foreign()");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, OneArg) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("abc"))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, TooManyArgs) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("abc", 1, 2, 3))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, FirstParameterCharSlice) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign(123, *i64))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, NonConstantType) {
  test::TestModule mod;
  mod.AppendCode(R"(
  f := () -> ()
  )");
  auto const *call  = mod.Append<ast::Call>(R"(foreign("f", f))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, SecondParameterType) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>(R"(foreign("f", 3))");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

// TODO: Slice tests.

TEST(BuiltinOpaque, Success) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("opaque()");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinOpaque, Arguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("opaque(3)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, Success) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes(bool)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinBytes, NoArguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes()");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, TooManyArguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes(i32, u8)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, NamedArgument) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes(T = i32)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, WrongType) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("bytes(3)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, Success) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment(bool)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinAlignment, NoArguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment()");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, TooManyArguments) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment(i32, u8)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, NamedArgument) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment(T = i32)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, WrongType) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("alignment(3)");
  type::QualType qt = mod.context().qual_types(call)[0];
  EXPECT_EQ(qt, type::QualType::Constant(type::U64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(Call, Uncallable) {
  test::TestModule mod;
  auto const *call  = mod.Append<ast::Call>("3()");
  type::QualType qt = mod.context().qual_types(call)[0];
  ASSERT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncallable-expression")));
}


TEST(Call, CrossModuleCallsWithoutADLGenerateErrors) {
 auto [imported_id, imported, inserted] =
      ir::ModuleId::FromFile<compiler::LibraryModule>(
          frontend::CanonicalFileName::Make(frontend::FileName{"imported1"}));

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported1")))
      .WillByDefault([id = imported_id](std::string_view) { return id; });

  frontend::StringSource src(R"(
  #{export} S ::= struct {}
  #{export} f ::= (s: S) => 3
  )");
  imported->AppendNodes(frontend::Parse(src, mod.consumer), mod.consumer,
                        mod.importer);

  mod.AppendCode(R"(
    mod ::= import "imported1"
    s: mod.S
    f(s)
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "undeclared-identifier")));
}

TEST(Call, CrossModuleWithADLSucceed) {
 auto [imported_id, imported, inserted] =
      ir::ModuleId::FromFile<compiler::LibraryModule>(
          frontend::CanonicalFileName::Make(frontend::FileName{"imported2"}));

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported2")))
      .WillByDefault([id = imported_id](std::string_view) { return id; });

  frontend::StringSource src(R"(
  #{export} S ::= struct {}
  #{export} f ::= (s: S) => 3
  )");
  imported->AppendNodes(frontend::Parse(src, mod.consumer), mod.consumer,
                        mod.importer);

  mod.AppendCode(R"(
    mod ::= import "imported2"
    f ::= (n: i64) => true
    s: mod.S
    s'f
  )");
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Call, CrossModuleWithADLWithoutExport) {
 auto [imported_id, imported, inserted] =
      ir::ModuleId::FromFile<compiler::LibraryModule>(
          frontend::CanonicalFileName::Make(frontend::FileName{"imported3"}));

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported3")))
      .WillByDefault([id = imported_id](std::string_view) { return id; });

  frontend::StringSource src(R"(
  #{export} S ::= struct {}
  f ::= (s: S) => 3
  )");
  imported->AppendNodes(frontend::Parse(src, mod.consumer), mod.consumer,
                        mod.importer);

  mod.AppendCode(R"(
    mod ::= import "imported3"
    s: mod.S
    s'f
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "undeclared-identifier")));
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
  test::TestModule mod;
  mod.AppendCode(context);
  auto const *e     = mod.Append<ast::Expression>(expr);
  auto qts          = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(expected_qual_type));
  EXPECT_THAT(mod.consumer.diagnostics(), expected_diagnostics);
}

INSTANTIATE_TEST_SUITE_P(
    All, CallTest,
    testing::ValuesIn({
        TestCase{
            .context            = "f ::= () => 3",
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
            .context            = "f ::= (x: $x) => x",
            .expr               = "f(3)",
            .expected_qual_type = type::QualType::NonConstant(type::I64),
        },
        TestCase{
            .context            = "f ::= (x: $x) => x",
            .expr               = "f(true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (x: $x) => x",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "S ::= struct (N :: i64) {}",
            .expr               = "S(n :: i64)",
            .expected_qual_type = type::QualType::Constant(type::Interface),
        },
    }));

}  // namespace
}  // namespace compiler
