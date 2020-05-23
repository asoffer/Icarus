#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(StructLiteral, SuccessEmpty) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::StructLiteral>(R"(struct {}
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(StructLiteral, SuccessNonEmpty) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::StructLiteral>(R"(struct {
    n: int64
    b := true
  }
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(StructLiteral, FieldError) {
  test::TestModule mod;
  auto const *s  = mod.Append<ast::StructLiteral>(R"(struct {
    n: 3
    b := true
  }
  )");
  auto const *qt = mod.data().qual_type(s);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

}  // namespace
}  // namespace compiler
