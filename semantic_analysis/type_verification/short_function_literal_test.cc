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
using ::testing::Pointee;

TEST(ShortFunctionLiteralTest, NoParameters) {
  ASSERT_THAT(Parsed<ast::ShortFunctionLiteral>("() => true"),
              Pointee(HasQualTypes(ElementsAre(
                  type::QualType::Constant(type::Func({}, {type::Bool}))))));
}

}  // namespace
}  // namespace semantic_analysis
