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
  auto const *call =
      mod.Append<ast::Call>(R"(
      foreign(name = "my_function", foreign_type = int64 -> bool)
      )");
  auto const *qt = mod.data().qual_type(call);
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
  auto const *qt = mod.data().qual_type(call);
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
  auto const *qt = mod.data().qual_type(call);
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
  auto const *qt = mod.data().qual_type(call);
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

// TODO: Non-builtin call expressions tests. Can't implement them yet because
// the verification is done as part of code emission.

}  // namespace
}  // namespace compiler
