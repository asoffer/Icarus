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
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"("abc"[0])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Char, type::Qualifiers::Constant() |
                                       type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, SliceNonConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  n: i64
  "abc"[n]
  )");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::Char, type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantSliceConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  s := "abc"
  s[0]
  )");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::Char, type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantSliceNonConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  s := "abc"
  n := 0
  s[n]
  )");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::Char, type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, SliceInvalidIndexType) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"("abc"["def"])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Char, type::Qualifiers::Constant() |
                                       type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-index-type")));
}

TEST(Index, SliceOutOfBoundsNegative) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"("abc"[-1])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Char, type::Qualifiers::Constant() |
                                       type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, SliceOutOfBoundsLarge) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"("abc"[4])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Char, type::Qualifiers::Constant() |
                                       type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, ArrayConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([1, 2, 3][0])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::Integer, type::Qualifiers::Constant())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, ArrayNonConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  n: i64
  [1 as i64, 2 as i64, 3 as i64][n]
  )");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::I64, type::Qualifiers::Unqualified())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantArrayConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  s := [1 as i64, 2 as i64, 3 as i64]
  s[0]
  )");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantArrayNonConstantIndex) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  s := [1 as i64, 2 as i64, 3 as i64]
  n := 0
  s[n]
  )");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType(type::I64, type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Index, ArrayInvalidIndexType) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([1 as i64]["def"])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::I64, type::Qualifiers::Constant())));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-index-type")));
}

TEST(Index, ArrayOutOfBoundsNegative) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([1 as i64][-1])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::I64, type::Qualifiers::Constant())));
  EXPECT_THAT(infra.diagnostics(), UnorderedElementsAre(Pair(
                                       "value-error", "negative-array-index")));
}

TEST(Index, ArrayOutOfBoundsLarge) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([1 as i64][3])");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                       type::I64, type::Qualifiers::Constant())));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-error", "indexing-array-out-of-bounds")));
}

TEST(Index, OverloadSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  S ::= struct {}
  __index__ ::= (s: *S, x: f64) -> bool { return true }

  thing: S
  thing[3.14]
  )");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

// TODO: We have not done error-handling yet (for any operator overload). We
// simply are assuming that if we find exactly one overload it is correct.

// TODO: BufferPtr tests

}  // namespace
}  // namespace compiler
