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
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  n: i64
  n = 0
  )");
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Assignment, NonReference) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  f ::= () => 3
  f() = 4
  )");
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "assigning-to-non-reference")));
}

TEST(Assignment, Constant) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  n ::= 3
  n = 0
  )");
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "assigning-to-constant")));
}

TEST(Assignment, WithCast) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  n: [*]i64
  m: *i64
  n = null
  (m, n) = (n, null)
  )");
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Assignment, TypeMismatch) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  n := 3
  n = "hello"
  )");
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "assignment-type-mismatch")));
}

// TODO: Significantly more tests covering value category and expression
// expansion.

}  // namespace
}  // namespace compiler
