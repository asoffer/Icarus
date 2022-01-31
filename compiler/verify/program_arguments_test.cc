#include "compiler/compiler.h"
#include "compiler/module.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::ElementsAre;
using ::testing::Eq;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::SizeIs;
using ::testing::UnorderedElementsAre;

TEST(ProgramArguments, Access) {
  test::TestModule mod;
  auto const *arguments  = mod.Append<ast::ProgramArguments>(R"(arguments)");
  auto qts               = mod.context().qual_types(arguments);
  EXPECT_THAT(qts, ElementsAre(type::QualType::NonConstant(
                       type::Slc(type::Slc(type::Char)))));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ProgramArguments, AccessibleInLocalScope) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(
  if (true) {
    arguments[0]
  }
  )");
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ProgramArguments, InaccessibleBehindScopeBoundary) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(
  () -> () {
    arguments[0]
  }
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("access-error", "program-arguments-access")));
}

TEST(ProgramArguments, CannotUseAsIdentifier) {
  test::TestModule mod;
  // Behind a function `arguments` should be inaccessible, but the keyword
  // should still be treated as reserved.
  mod.Append<ast::Node>(R"(
  () -> () {
    arguments := 3
  }
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("parse-error", "declaring-non-identifier")));
}

}  // namespace
}  // namespace compiler


