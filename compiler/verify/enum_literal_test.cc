#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(Enum, Success) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"(enum { A \\ B \\ C })");
  auto const *e = mod.get<ast::EnumLiteral>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Enum, NonIntegralEnumerator) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"(enum { A \\ B ::= "x" \\ C ::= 3.1 })");
  auto const *e = mod.get<ast::EnumLiteral>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-integral-enumerator"),
                           Pair("type-error", "non-integral-enumerator")));
}

TEST(Enum, NonConstantEnumerator) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"(
  n := 3
  enum { A \\ B \\ C ::= n }
  )");
  auto const *e = mod.get<ast::EnumLiteral>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-constant-enumerator")));
}

TEST(Flags, Success) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"(flags { A \\ B \\ C })");
  auto const *e = mod.get<ast::EnumLiteral>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Flags, NonIntegralEnumerator) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"(flags { A \\ B ::= "x" \\ C ::= 3.1 })");
  auto const *e = mod.get<ast::EnumLiteral>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-integral-enumerator"),
                           Pair("type-error", "non-integral-enumerator")));
}

TEST(Flags, NonConstantEnumerator) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"(
  n := 3
  enum { A \\ B \\ C ::= n }
  )");
  auto const *e = mod.get<ast::EnumLiteral>();
  auto qts      = mod.context().qual_types(e);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-constant-enumerator")));
}

// TODO: Handle situations where the type is specified in the declaration.

}  // namespace
}  // namespace compiler
