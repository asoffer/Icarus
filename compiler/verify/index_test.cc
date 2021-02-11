#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(Index, SliceConstantIndex) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[0])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Char, type::Quals::Const() | type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, SliceNonConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(n: i64)");
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[n])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::Char, type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantSliceConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr = mod.Append<ast::Expression>(R"(s[0])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::Char, type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantSliceNonConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s := "abc"
  n := 0
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(s[n])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::Char, type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, SliceInvalidIndexType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc"["def"])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Char, type::Quals::Const() | type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-index-type")));
}

TEST(Index, SliceOutOfBoundsNegative) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[-1])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Char, type::Quals::Const() | type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, SliceOutOfBoundsLarge) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[4])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Char, type::Quals::Const() | type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, ArrayConstantIndex) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3][0])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Const())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, ArrayNonConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(n: i64)");
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3][n])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Unqualified())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantArrayConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(s := [1, 2, 3])");
  auto const *expr = mod.Append<ast::Expression>(R"(s[0])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType(type::I64, type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantArrayNonConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s := [1, 2, 3]
  n := 0
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(s[n])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType(type::I64, type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, ArrayInvalidIndexType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3]["def"])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Const())));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-index-type")));
}

TEST(Index, ArrayOutOfBoundsNegative) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3][-1])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Const())));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "negative-array-index")));
}

TEST(Index, ArrayOutOfBoundsLarge) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3][3])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Const())));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "indexing-array-out-of-bounds")));
}

// TODO: BufferPtr tests

}  // namespace
}  // namespace compiler
