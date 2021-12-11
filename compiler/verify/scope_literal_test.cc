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
  auto const *s = mod.Append<ast::ScopeLiteral>(R"(scope {}
  )");
  auto qts      = mod.context().qual_types(s);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Scp({}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ScopeLiteral, StatelessNonConstantMember) {
  test::TestModule mod;
  auto const *s = mod.Append<ast::ScopeLiteral>(R"(scope {
    enter := jump () {}
  }
  )");
  auto qts      = mod.context().qual_types(s);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Scp({}))));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-constant-scope-member")));
}

TEST(ScopeLiteral, StatelessMemberTypes) {
  test::TestModule mod;
  auto const *s = mod.Append<ast::ScopeLiteral>(R"(scope {
    enter ::= 3
    exit ::= 3
  }
  )");
  auto qts      = mod.context().qual_types(s);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Scp({}))));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "non-jump-enter"),
                                   Pair("type-error", "non-callable-exit")));
}

TEST(ScopeLiteral, StatelessInitIsStateful) {
  test::TestModule mod;
  auto const *s = mod.Append<ast::ScopeLiteral>(R"(scope {
    enter ::= jump [state: *i64] () {}
  }
  )");
  auto qts      = mod.context().qual_types(s);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Scp({}))));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "state-type-mismatch")));
}

TEST(ScopeLiteral, SuccessStateful) {
  test::TestModule mod;
  auto const *s = mod.Append<ast::ScopeLiteral>(R"(scope {}
  )");
  auto qts      = mod.context().qual_types(s);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Scp({}))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ScopeLiteral, StatefulNonConstantMember) {
  test::TestModule mod;
  auto const *s = mod.Append<ast::ScopeLiteral>(R"(scope (i64) {
    enter := jump [state: *i64] () {}
  }
  )");
  auto qts      = mod.context().qual_types(s);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Scp({}))));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-constant-scope-member")));
}

TEST(ScopeLiteral, StatefulMemberTypes) {
  test::TestModule mod;
  auto const *s = mod.Append<ast::ScopeLiteral>(R"(scope (i64) {
    enter ::= 3
    exit ::= 3
  }
  )");
  auto qts      = mod.context().qual_types(s);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Scp({}))));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "non-jump-enter"),
                                   Pair("type-error", "non-callable-exit")));
}

TEST(ScopeLiteral, StatefulInitIsStateless) {
  test::TestModule mod;
  auto const *s = mod.Append<ast::ScopeLiteral>(R"(scope (i64) {
    enter ::= jump () {}
  }
  )");
  auto qts      = mod.context().qual_types(s);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Scp({}))));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "state-type-mismatch")));
}

}  // namespace
}  // namespace compiler
