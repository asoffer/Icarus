#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(Access, EnumSuccess) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(E ::= enum { A \\ B \\ C })");
  auto const *enumerator = mod.Append<ast::Expression>(R"(E.A)");
  type::QualType qt      = mod.context().qual_type(enumerator);
  EXPECT_TRUE(qt.type().is<type::Enum>());
  EXPECT_EQ(qt.quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, EnumMisnamed) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(E ::= enum { A \\ B \\ C })");
  auto const *enumerator = mod.Append<ast::Expression>(R"(E.D)");
  type::QualType qt      = mod.context().qual_type(enumerator);
  EXPECT_TRUE(qt.type().is<type::Enum>());
  EXPECT_EQ(qt.quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, FlagsSuccess) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(F ::= flags { A \\ B \\ C })");
  auto const *flag  = mod.Append<ast::Expression>(R"(F.A)");
  type::QualType qt = mod.context().qual_type(flag);
  EXPECT_TRUE(qt.type().is<type::Flags>());
  EXPECT_EQ(qt.quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, FlagsMisnamed) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(F ::= flags { A \\ B \\ C })");
  auto const *flag  = mod.Append<ast::Expression>(R"(F.D)");
  type::QualType qt = mod.context().qual_type(flag);
  EXPECT_TRUE(qt.type().is<type::Flags>());
  EXPECT_EQ(qt.quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, NonConstantType) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(T := i64)");
  auto const *expr  = mod.Append<ast::Expression>(R"(T.something)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-constant-type-member-access")));
}

// TODO: Test covering an evaluation error when accessing a type member.

TEST(Access, TypeHasNoMembers) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(T ::= i64)");
  auto const *expr  = mod.Append<ast::Expression>(R"(T.something)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "type-has-no-members")));
}

TEST(Access, AccessStructField) {
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: i64
    b: bool
  }
  non_constant: S
  constant :: S
  )");
  auto const *non_constant = mod.Append<ast::Expression>(R"(non_constant.n)");
  type::QualType non_constant_qt = mod.context().qual_type(non_constant);
  auto const *constant           = mod.Append<ast::Expression>(R"(constant.n)");
  type::QualType constant_qt     = mod.context().qual_type(constant);
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  EXPECT_EQ(non_constant_qt, type::QualType(type::I64, type::Quals::Ref()));
  EXPECT_EQ(constant_qt, type::QualType(type::I64, type::Quals::Ref() |
                                                        type::Quals::Const()));
}

TEST(Access, NoFieldInStruct) {
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: i64
    b: bool
  }
  s: S
  )");
  auto const *expr  = mod.Append<ast::Expression>(R"(s.x)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, ConstantSliceLength) {
  test::TestModule mod;
  auto const *expr  = mod.Append<ast::Expression>(R"("abc".length)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(
      qt, type::QualType(type::U64, type::Quals::Ref() | type::Quals::Const()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, NonConstantSliceLength) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr  = mod.Append<ast::Expression>(R"(s.length)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType(type::U64, type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, NonConstantSliceData) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr  = mod.Append<ast::Expression>(R"(s.data)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType(type::BufPtr(type::Char), type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, SliceInvalidMember) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr  = mod.Append<ast::Expression>(R"(s.size)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, ArrayLength) {
  test::TestModule mod;
  auto const *expr  = mod.Append<ast::Expression>(R"([3; i64].length)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Constant(type::Get<type::Array::length_t>()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, MultidimensionalArrayLength) {
  test::TestModule mod;
  auto const *expr  = mod.Append<ast::Expression>(R"([3, 2; i64].length)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Constant(type::Get<type::Array::length_t>()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, ArrayElementType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([3, 2; i64].element_type)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, ArrayInvalidMember) {
  test::TestModule mod;
  auto const *expr  = mod.Append<ast::Expression>(R"([3; i64].size)");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

// TODO: Field not exported from another module.
// TODO: Non-constant module
// TODO: Module evaluation failure
// TODO: Undeclared identifier across module boundaries
// TODO: Type error across module boundaries (other module already generated
// error)
// TODO: Valid access across module boundaries
// TODO: Valid overload set across module boundary
// TODO: Valid scope set across module boundary
// TODO: Valid mix of overloads and scopes across module boundary
// TODO: Invalid overload set across module boundaries
// TODO: Error on accessing incomplete member.

}  // namespace
}  // namespace compiler
