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
  EXPECT_THAT(YieldArgumentTypes(mod.context(), &block), IsEmpty());
}

TEST(BlockNode, HasJump) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s ::= scope (int64) {
    b ::= block {}
  }
  )");
  absl::Span<ast::BlockNode const> blocks =
      mod.Append<ast::ScopeNode>(R"(s () b {
        value := true
        << value
      }
      )")->blocks();
  ASSERT_THAT(blocks, SizeIs(1));
  auto const &block = blocks[0];
  EXPECT_THAT(mod.context().qual_type(&block),
              Pointee(type::QualType::Constant(type::Block)));
  EXPECT_THAT(YieldArgumentTypes(mod.context(), &block),
              UnorderedElementsAre(core::Arguments<type::QualType>(
                  {type::QualType(type::Bool, type::Quals::Ref())}, {})));
}

TEST(BlockNode, LabeledJumpSkipBlock) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s ::= scope (int64) {
    b ::= block {}
    exit ::= (b: bool) -> () {}
  }
  )");
  absl::Span<ast::BlockNode const> blocks =
      mod.Append<ast::ScopeNode>(R"(#.my_label s () b { #.my_label << true })")
          ->blocks();
  ASSERT_THAT(blocks, SizeIs(1));
  auto const &block = blocks[0];
  EXPECT_THAT(mod.context().qual_type(&block),
              Pointee(type::QualType::Constant(type::Block)));
  EXPECT_THAT(YieldArgumentTypes(mod.context(), &block), IsEmpty());
}

// TODO:
// * Case where `done` is generic.
// * Case where `done` doesn't match arguments provided.

}  // namespace
}  // namespace compiler
