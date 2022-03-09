#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(ArrayType, Correct) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"([3; i64])");
    auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::Constant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"([2; [3; i64]])");
    auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::Constant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"([1, 2, 3; i64])");
    auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::Constant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"([1 as i8; i64])");
    auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::Constant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

}

TEST(ArrayType, NonConstantType) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  T := i64
  [3; T]
  )");
  auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Type_)));

  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ArrayType, NonTypeElement) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"([3; 2])");
  auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));

  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "array-data-type-not-a-type")));
}

TEST(ArrayType, NonConstantLength) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(n := 3

  [3, n, 2; i64]
  )");
  auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ArrayType, NonIntegerLength) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"([3.0; i64])");
  auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));

  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-integral-array-length")));
}

TEST(ArrayType, NonIntegerNonConstant) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(x := 3.0

  [x; i64]
  )");
  auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::Type_)));

  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-integral-array-length")));
}

TEST(ArrayType, InvalidPattern) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(3 ~ [2; `T])");
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("pattern-error", "non-type-array-type-match")));
}

TEST(ArrayType, ValidPattern) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    [2, 3; i64] ~ [`N; `T]
    N
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
    EXPECT_THAT(qts,
               UnorderedElementsAre(type::QualType::Constant(type::Integer)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    [2, 3; i64] ~ [`N; `T]

    T
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Expression>());
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::Constant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

}  // namespace
}  // namespace compiler
