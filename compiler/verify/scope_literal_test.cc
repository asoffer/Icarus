#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(ScopeLiteral, SuccessStateless) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::ScopeLiteral>(R"(scope {}
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(qt->type(), type::Scope);
  EXPECT_TRUE(qt->constant());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ScopeLiteral, StatelessNonConstantMember) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::ScopeLiteral>(R"(scope {
    init := jump () {}
  }
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(qt->type(), type::Scope);
  EXPECT_TRUE(qt->constant());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-constant-scope-member")));
}

TEST(ScopeLiteral, StatelessMemberTypes) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::ScopeLiteral>(R"(scope {
    init ::= 3
    done ::= 3
  }
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(qt->type(), type::Scope);
  EXPECT_TRUE(qt->constant());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(
        Pair("type-error", "non-jump-init"),
        Pair("type-error", "non-callable-done")));
}

TEST(ScopeLiteral, StatelessInitIsStateful) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::ScopeLiteral>(R"(scope {
    init ::= jump [state: *int64] () {}
  }
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(qt->type(), type::Scope);
  EXPECT_TRUE(qt->constant());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "state-type-mismatch")));
}

TEST(ScopeLiteral, SuccessStateful) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::ScopeLiteral>(R"(scope {}
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(qt->type(), type::Scope);
  EXPECT_TRUE(qt->constant());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ScopeLiteral, StatefulNonConstantMember) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::ScopeLiteral>(R"(scope (int64) {
    init := jump [state: *int64] () {}
  }
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(qt->type(), type::Scope);
  EXPECT_TRUE(qt->constant());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-constant-scope-member")));
}

TEST(ScopeLiteral, StatefulMemberTypes) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::ScopeLiteral>(R"(scope (int64) {
    init ::= 3
    done ::= 3
  }
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(qt->type(), type::Scope);
  EXPECT_TRUE(qt->constant());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(
        Pair("type-error", "non-jump-init"),
        Pair("type-error", "non-callable-done")));
}

TEST(ScopeLiteral, StatefulInitIsStateless) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::ScopeLiteral>(R"(scope (int64) {
    init ::= jump () {}
  }
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(qt->type(), type::Scope);
  EXPECT_TRUE(qt->constant());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "state-type-mismatch")));
}

}  // namespace
}  // namespace compiler