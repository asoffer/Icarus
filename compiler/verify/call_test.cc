#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(BuiltinForeign, FunctionSuccess) {
  test::TestModule mod;
  auto const *call =
      mod.Append<ast::Call>(R"(foreign("my_function", int64 -> bool))");
  auto const *qt = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(
      *qt, type::QualType::NonConstant(type::Func(
               {core::AnonymousParam(type::QualType::NonConstant(type::Int64))},
               {type::Bool})));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, PointerSuccess) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>(R"(foreign("my_ptr", *int64))");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Ptr(type::Int64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, BufferPointerSuccess) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>(R"(foreign("my_array", [*]int64))");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::BufPtr(type::Int64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinForeign, NamedArgs) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>(R"(
      foreign(name = "my_function", foreign_type = int64 -> bool)
      )");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, NoArgs) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("foreign()");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, OneArg) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>(R"(foreign("abc"))");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, TooManyArgs) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>(R"(foreign("abc", 1, 2, 3))");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, FirstParameterByteView) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>(R"(foreign(123, *int64))");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, NonConstantType) {
  test::TestModule mod;
  mod.AppendCode(R"(
  f := () -> ()
  )");
  auto const *call = mod.Append<ast::Call>(R"(foreign("f", f))");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinForeign, SecondParameterType) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>(R"(foreign("f", 3))");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinOpaque, Success) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("opaque()");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinOpaque, Arguments) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("opaque(3)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, Success) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("bytes(bool)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinBytes, NoArguments) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("bytes()");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, TooManyArguments) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("bytes(int32, nat8)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, NamedArgument) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("bytes(T = int32)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinBytes, WrongType) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("bytes(3)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, Success) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("alignment(bool)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(BuiltinAlignment, NoArguments) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("alignment()");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, TooManyArguments) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("alignment(int32, nat8)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, NamedArgument) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("alignment(T = int32)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(BuiltinAlignment, WrongType) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("alignment(3)");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Nat64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "builtin-error")));
}

TEST(Call, Uncallable) {
  test::TestModule mod;
  auto const *call = mod.Append<ast::Call>("3()");
  auto const *qt   = mod.data().qual_type(call);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "uncallable-expression")));
}

struct TestCase {
  std::string context;
  std::string expr;
  std::optional<type::QualType> expected_qual_type = std::nullopt;
  testing::Matcher<absl::Span<std::pair<std::string, std::string> const>>
      expected_diagnostics = IsEmpty();
};

// TODO: This is starting to get to the point where it should be shared test
// infrastructure.
using CallTest = testing::TestWithParam<TestCase>;
TEST_P(CallTest, Call) {
  auto const &[context, expr, expected_qual_type, expected_diagnostics] = GetParam();
  test::TestModule mod;
  mod.AppendCode(context);
  auto const *e  = mod.Append<ast::Expression>(expr);
  auto const *qt = mod.data().qual_type(e);
  if (expected_qual_type) {
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, *expected_qual_type);
  } else {
    EXPECT_EQ(qt, nullptr);
  }
  EXPECT_THAT(mod.consumer.diagnostics(), expected_diagnostics);
}

INSTANTIATE_TEST_SUITE_P(
    All, CallTest,
    testing::ValuesIn({
        TestCase{
            .context            = "f ::= () => 3",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::Int64),
        },
        TestCase{
            .context              = "f ::= () => 3",
            .expr                 = "f(true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: int64) => n",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::Int64),
        },
        TestCase{
            .context            = "f ::= (n: int64) => n",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::Int64),
        },
        TestCase{
            .context              = "f ::= (n: int64) => 3",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: int64) => 3",
            .expr                 = "f(b = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: int64) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 0) => n",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::Int64),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 0) => n",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::Int64),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 0) => n",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::Int64),
        },
        TestCase{
            .context              = "f ::= (n: int64 = 0) => 3",
            .expr                 = "f(b = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: int64 = 0) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: int64, b: bool) => b",
            .expr               = "f(4, true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (n: int64, b: bool) => 3",
            .expr                 = "f()",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: int64, b: bool) => 3",
            .expr                 = "f(0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: int64, b: bool) => 3",
            .expr                 = "f(true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: int64, b: bool) => 3",
            .expr                 = "f(n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: int64, b: bool) => 3",
            .expr                 = "f(b = true)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context              = "f ::= (n: int64, b: bool) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 3, b: bool = true) => b",
            .expr               = "f(4, true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 3, b: bool = true) => b",
            .expr               = "f(4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (n: int64 = 3, b: bool = true) => 3",
            .expr                 = "f(0, n = 0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 3, b: bool = true) => b",
            .expr               = "f(n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 3, b: bool = true) => b",
            .expr               = "f(b = true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 3, b: bool = true) => b",
            .expr               = "f(b = true, n = 4)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (n: int64 = 3, b: bool = true) => b",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = "f ::= (x: int64 | bool) => true",
            .expr               = "f(true)",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context              = "f ::= (x: int64 | bool) => true",
            .expr                 = "f(0.0)",
            .expected_diagnostics = UnorderedElementsAre(
                Pair("type-error", "uncallable-with-arguments")),
        },
        // Overload sets
        TestCase{
            .context            = R"(
            f ::= () => true
            f ::= (n: int64) => n
            )",
            .expr               = "f(0)",
            .expected_qual_type = type::QualType::NonConstant(type::Int64),
        },
        TestCase{
            .context            = R"(
            f ::= () => true
            f ::= (n: int64) => n
            )",
            .expr               = "f()",
            .expected_qual_type = type::QualType::NonConstant(type::Bool),
        },
        TestCase{
            .context            = R"(
            x: bool | int64 = 0
            f ::= (b: bool) => true
            f ::= (n: int64) => n
            )",
            .expr               = "f(x)",
            .expected_qual_type = type::QualType::NonConstant(
                type::Var({type::Bool, type::Int64})),
        },
        TestCase{
            .context            = R"(
            f ::= (b: bool) => true
            f ::= (n: int64) => n
            )",
            .expr               = "f(n = 0)",
            .expected_qual_type = type::QualType::NonConstant(type::Int64),
        },
    }));

}  // namespace
}  // namespace compiler
