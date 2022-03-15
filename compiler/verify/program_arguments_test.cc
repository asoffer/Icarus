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
  test::CompilerInfrastructure infra;
  auto &mod             = infra.add_module(R"(arguments)");
  auto const *arguments = mod.get<ast::ProgramArguments>();
  auto qts              = mod.context().qual_types(arguments);
  EXPECT_THAT(qts, ElementsAre(type::QualType::NonConstant(
                       type::Slc(type::Slc(type::Char)))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ProgramArguments, AccessibleInLocalScope) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  if (true) {
    arguments[0]
  }
  )");
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(ProgramArguments, InaccessibleBehindScopeBoundary) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  () -> () {
    arguments[0]
  }
  )");
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("access-error", "program-arguments-access")));
}

TEST(ProgramArguments, CannotUseAsIdentifier) {
  // Behind a function `arguments` should be inaccessible, but the keyword
  // should still be treated as reserved.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  () -> () {
    arguments := 3
  }
  )");
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("parse-error", "declaring-non-identifier")));
}

}  // namespace
}  // namespace compiler

