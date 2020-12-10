#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(Assignment, SimpleSuccess) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n: i64
  n = 0
  )");
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Assignment, NonReference) {
  test::TestModule mod;
  mod.AppendCode(R"(
  f ::= () => 3
  f() = 4
  )");
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "assigning-to-non-reference")));
}

TEST(Assignment, Constant) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n ::= 3
  n = 0
  )");
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "assigning-to-constant")));
}

TEST(Assignment, TypeMismatch) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n := 3
  n = "hello"
  )");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "assignment-type-mismatch")));
}

// TODO: Significantly more tests covering value category and expression
// expansion.

}  // namespace
}  // namespace compiler
