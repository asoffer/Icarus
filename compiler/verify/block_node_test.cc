#include "compiler/compiler.h"
#include "compiler/verify/common.h"
#include "core/arguments.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::SizeIs;
using ::testing::UnorderedElementsAre;

TEST(BlockNode, NoJumps) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s ::= scope (i64) {
    b ::= block {}
  }
  )");
  absl::Span<ast::BlockNode const> blocks =
      mod.Append<ast::ScopeNode>(R"(s () b {})")->blocks();
  ASSERT_THAT(blocks, SizeIs(1));
  auto const &block = blocks[0];
  EXPECT_EQ(mod.context().qual_types(&block)[0],
            type::QualType::Constant(type::Block));
  EXPECT_THAT(YieldArgumentTypes(mod.context(), &block), IsEmpty());
}

TEST(BlockNode, HasJump) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s ::= scope (i64) {
    b ::= block {}
  }
  )");
  absl::Span<ast::BlockNode const> blocks =
      mod.Append<ast::ScopeNode>(R"(s () b {
        value := true
        << value
      }
  )")
          ->blocks();
  ASSERT_THAT(blocks, SizeIs(1));
  auto const &block = blocks[0];
  EXPECT_EQ(mod.context().qual_types(&block)[0],
            type::QualType::Constant(type::Block));
  EXPECT_THAT(YieldArgumentTypes(mod.context(), &block),
              UnorderedElementsAre(core::Arguments<type::QualType>(
                  {type::QualType(type::Bool, type::Quals::Ref())}, {})));
}

TEST(BlockNode, LabeledJumpSkipBlock) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s ::= scope (i64) {
    b ::= block {}
    exit ::= (b: bool) -> () {}
  }
  )");
  absl::Span<ast::BlockNode const> blocks =
      mod.Append<ast::ScopeNode>(R"(#.my_label s () b { #.my_label << true })")
          ->blocks();
  ASSERT_THAT(blocks, SizeIs(1));
  auto const &block = blocks[0];
  EXPECT_EQ(mod.context().qual_types(&block)[0],
            type::QualType::Constant(type::Block));
  EXPECT_THAT(YieldArgumentTypes(mod.context(), &block), IsEmpty());
}

TEST(BlockNode, NoBlock) {
  test::TestModule mod;
  mod.AppendCode(R"(
  some_scope ::= scope {}
  some_scope () not_a_block {}
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("reference-error", "no-block-with-name")));
}

// TODO:
// * Case where `exit` is generic.
// * Case where `exit` doesn't match arguments provided.

}  // namespace
}  // namespace compiler
