#include "compiler/compiler.h"
#include "core/arguments.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pointee;
using ::testing::SizeIs;
using ::testing::UnorderedElementsAre;

TEST(BlockNode, NoJumps) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s ::= scope (int64) {
    b ::= block {}
  }
  )");
  absl::Span<ast::BlockNode const> blocks =
      mod.Append<ast::ScopeNode>(R"(s () b {})")->blocks();
  ASSERT_THAT(blocks, SizeIs(1));
  auto const &block = blocks[0];
  EXPECT_THAT(mod.context().qual_type(&block),
              Pointee(type::QualType::Constant(type::Block)));
  EXPECT_THAT(mod.context().yield_types(&block), IsEmpty());
}

TEST(BlockNode, HasJump) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s ::= scope (int64) {
    b ::= block {}
  }
  )");
  absl::Span<ast::BlockNode const> blocks =
      mod.Append<ast::ScopeNode>(R"(s () b { << true })")->blocks();
  ASSERT_THAT(blocks, SizeIs(1));
  auto const &block = blocks[0];
  EXPECT_THAT(mod.context().qual_type(&block),
              Pointee(type::QualType::Constant(type::Block)));
  EXPECT_THAT(
      mod.context().yield_types(&block),
      UnorderedElementsAre(core::Arguments<type::Type>({type::Bool}, {})));
}

TEST(BlockNode, LabeledJumpSkipBlock) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s ::= scope (int64) {
    b ::= block {}
  }
  )");
  absl::Span<ast::BlockNode const> blocks =
      mod.Append<ast::ScopeNode>(R"(#.my_label s () b { #.my_label << true })")
          ->blocks();
  ASSERT_THAT(blocks, SizeIs(1));
  auto const &block = blocks[0];
  EXPECT_THAT(mod.context().qual_type(&block),
              Pointee(type::QualType::Constant(type::Block)));
  EXPECT_THAT(mod.context().yield_types(&block), IsEmpty());
}


}  // namespace
}  // namespace compiler
