#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(Index, ByteViewConstantIndex) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[0])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Nat8,
                                type::Quals::Const() | type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, ByteViewNonConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(n: int64)");
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[n])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Nat8, type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantByteViewConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr = mod.Append<ast::Expression>(R"(s[0])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Nat8, type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantByteViewNonConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s := "abc"
  n := 0
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(s[n])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Nat8, type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, ByteViewInvalidIndexType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc"["def"])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Nat8,
                                type::Quals::Const() | type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-index-type")));
}

TEST(Index, ByteViewOutOfBoundsNegative) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[-1])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Nat8,
                                type::Quals::Const() | type::Quals::Buf()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "negative-string-index")));
}

TEST(Index, ByteViewOutOfBoundsLarge) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc"[3])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Nat8,
                                type::Quals::Const() | type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "indexing-string-out-of-bounds")));
}

TEST(Index, ArrayConstantIndex) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3][0])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Const()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, ArrayNonConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(n: int64)");
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3][n])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Unqualified()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantArrayConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(s := [1, 2, 3])");
  auto const *expr = mod.Append<ast::Expression>(R"(s[0])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, NonConstantArrayNonConstantIndex) {
  test::TestModule mod;
  mod.AppendCode(R"(
  s := [1, 2, 3]
  n := 0
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(s[n])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Index, ArrayInvalidIndexType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3]["def"])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Const()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-index-type")));
}

TEST(Index, ArrayOutOfBoundsNegative) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3][-1])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Const()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "negative-array-index")));
}

TEST(Index, ArrayOutOfBoundsLarge) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([1, 2, 3][3])");
  auto const *qt   = mod.data().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Const()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "indexing-array-out-of-bounds")));
}

// TODO: BufferPtr tests

}  // namespace
}  // namespace compiler
