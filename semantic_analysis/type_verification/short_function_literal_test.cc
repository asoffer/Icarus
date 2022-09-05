#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/slice.h"
#include "semantic_analysis/type_verification/matchers.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/primitive.h"
#include "type/slice.h"

namespace semantic_analysis {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;

TEST(ShortFunctionLiteral, NoParameters) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"(() => true)");
  EXPECT_THAT(
      infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
      ElementsAre(type::QualType::Constant(type::Func({}, {type::Bool}))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

}  // namespace
}  // namespace semantic_analysis
