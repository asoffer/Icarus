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
  auto const *decl = mod.Append<ast::Node>(R"(E ::= enum { A \\ B \\ C })");
  auto const *enumerator = mod.Append<ast::Expression>(R"(E.A)");
  auto const *qt         = mod.data().qual_type(enumerator);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type()->is<type::Enum>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, EnumMisnamed) {
  test::TestModule mod;
  auto const *decl = mod.Append<ast::Node>(R"(E ::= enum { A \\ B \\ C })");
  auto const *enumerator = mod.Append<ast::Expression>(R"(E.D)");
  auto const *qt         = mod.data().qual_type(enumerator);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type()->is<type::Enum>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, FlagsSuccess) {
  test::TestModule mod;
  auto const *decl = mod.Append<ast::Node>(R"(F ::= flags { A \\ B \\ C })");
  auto const *flag = mod.Append<ast::Expression>(R"(F.A)");
  auto const *qt   = mod.data().qual_type(flag);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type()->is<type::Flags>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, FlagsMisnamed) {
  test::TestModule mod;
  auto const *decl = mod.Append<ast::Node>(R"(F ::= flags { A \\ B \\ C })");
  auto const *flag = mod.Append<ast::Expression>(R"(F.D)");
  auto const *qt   = mod.data().qual_type(flag);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type()->is<type::Flags>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, NonConstantType) {
  test::TestModule mod;
  auto const *decl = mod.Append<ast::Node>(R"(T := int64)");
  auto const *expr = mod.Append<ast::Expression>(R"(T.something)");
  auto const *qt   = mod.data().qual_type(expr);
  EXPECT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-constant-type-member-access")));
}

// TODO: More tests.

}  // namespace
}  // namespace compiler
