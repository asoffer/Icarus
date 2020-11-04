#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Pointee;
using ::testing::UnorderedElementsAre;

TEST(Access, EnumSuccess) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(E ::= enum { A \\ B \\ C })");
  auto const *enumerator = mod.Append<ast::Expression>(R"(E.A)");
  auto const *qt         = mod.context().qual_type(enumerator);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type()->is<type::Enum>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, EnumMisnamed) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(E ::= enum { A \\ B \\ C })");
  auto const *enumerator = mod.Append<ast::Expression>(R"(E.D)");
  auto const *qt         = mod.context().qual_type(enumerator);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type()->is<type::Enum>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, FlagsSuccess) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(F ::= flags { A \\ B \\ C })");
  auto const *flag = mod.Append<ast::Expression>(R"(F.A)");
  auto const *qt   = mod.context().qual_type(flag);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type()->is<type::Flags>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, FlagsMisnamed) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(F ::= flags { A \\ B \\ C })");
  auto const *flag = mod.Append<ast::Expression>(R"(F.D)");
  auto const *qt   = mod.context().qual_type(flag);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type()->is<type::Flags>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, NonConstantType) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(T := int64)");
  auto const *expr = mod.Append<ast::Expression>(R"(T.something)");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-constant-type-member-access")));
}

// TODO: Test covering an evaluation error when accessing a type member.

TEST(Access, TypeHasNoMembers) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(T ::= int64)");
  auto const *expr = mod.Append<ast::Expression>(R"(T.something)");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "type-has-no-members")));
}

TEST(Access, AccessStructField) {
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: int64
    b: bool
  }
  non_constant: S
  constant :: S
  )");
  auto const *non_constant = mod.Append<ast::Expression>(R"(non_constant.n)");
  auto const *non_constant_qt = mod.context().qual_type(non_constant);
  auto const *constant        = mod.Append<ast::Expression>(R"(constant.n)");
  auto const *constant_qt     = mod.context().qual_type(constant);
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  ASSERT_NE(non_constant_qt, nullptr);
  EXPECT_EQ(*non_constant_qt, type::QualType(type::Int64, type::Quals::Ref()));
  ASSERT_NE(constant_qt, nullptr);
  EXPECT_EQ(
      *constant_qt,
      type::QualType(type::Int64, type::Quals::Ref() | type::Quals::Const()));
}

TEST(Access, NoFieldInStruct) {
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: int64
    b: bool
  }
  s: S
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(s.x)");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, ConstantByteViewLength) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc".length)");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::Constant(type::Nat64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, NonConstantByteViewLength) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr = mod.Append<ast::Expression>(R"(s.length)");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_THAT(qt, Pointee(type::QualType::NonConstant(type::Nat64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, ByteViewInvalidMember) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr = mod.Append<ast::Expression>(R"(s.size)");
  auto const *qt   = mod.context().qual_type(expr);
  EXPECT_EQ(qt, nullptr);
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
