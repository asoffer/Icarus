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
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(n: i64)");
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[n])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::Char, type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantSliceConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(s := "abc")");
  auto const *expr = mod.Append<ast::Expression>(R"(s[0])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::Char, type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantSliceNonConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
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
                       type::QualType(type::Integer, type::Quals::Const())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, ArrayNonConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(n: i64)");
  auto const *expr =
      mod.Append<ast::Expression>(R"([1 as i64, 2 as i64, 3 as i64][n])");
  auto qts = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Unqualified())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantArrayConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(s := [1 as i64, 2 as i64, 3 as i64])");
  auto const *expr = mod.Append<ast::Expression>(R"(s[0])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType(type::I64, type::Quals::Buf())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantArrayNonConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  s := [1 as i64, 2 as i64, 3 as i64]
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
  auto const *expr = mod.Append<ast::Expression>(R"([1 as i64]["def"])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Const())));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-index-type")));
}

TEST(Index, ArrayOutOfBoundsNegative) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1 as i64][-1])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Const())));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("value-error", "negative-array-index")));
}

TEST(Index, ArrayOutOfBoundsLarge) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1 as i64][3])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Quals::Const())));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-error", "indexing-array-out-of-bounds")));
}

TEST(Index, OverloadSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  S ::= struct {}
  __index__ ::= (s: *S, x: f64) -> bool { return true }

  thing: S
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(thing[3.14])");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

// TODO: We have not done error-handling yet (for any operator overload). We
// simply are assuming that if we find exactly one overload it is correct.

// TODO: BufferPtr tests

}  // namespace
}  // namespace compiler
