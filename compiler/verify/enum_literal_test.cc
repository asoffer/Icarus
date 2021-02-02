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
  test::TestModule mod;
  auto const *e  = mod.Append<ast::EnumLiteral>(R"(enum { A \\ B \\ C })");
  type::QualType qt = mod.context().qual_type(e);
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Enum, NonIntegralEnumerator) {
  test::TestModule mod;
  auto const *e =
      mod.Append<ast::EnumLiteral>(R"(enum { A \\ B ::= "x" \\ C ::= 3.1 })");
  type::QualType qt = mod.context().qual_type(e);
  mod.compiler.CompleteWorkQueue();
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-integral-enumerator"),
                           Pair("type-error", "non-integral-enumerator")));
}

TEST(Enum, NonConstantEnumerator) {
  test::TestModule mod;
  mod.AppendCode("n := 3");
  auto const *e = mod.Append<ast::EnumLiteral>(R"(enum { A \\ B \\ C ::= n })");
  type::QualType qt = mod.context().qual_type(e);
  mod.compiler.CompleteWorkQueue();
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-constant-enumerator")));
}

TEST(Flags, Success) {
  test::TestModule mod;
  auto const *e  = mod.Append<ast::EnumLiteral>(R"(flags { A \\ B \\ C })");
  type::QualType qt = mod.context().qual_type(e);
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Flags, NonIntegralEnumerator) {
  test::TestModule mod;
  auto const *e =
      mod.Append<ast::EnumLiteral>(R"(flags { A \\ B ::= "x" \\ C ::= 3.1 })");
  type::QualType qt = mod.context().qual_type(e);
  mod.compiler.CompleteWorkQueue();
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-integral-enumerator"),
                           Pair("type-error", "non-integral-enumerator")));
}

TEST(Flags, NonConstantEnumerator) {
  test::TestModule mod;
  mod.AppendCode("n := 3");
  auto const *e = mod.Append<ast::EnumLiteral>(R"(enum { A \\ B \\ C ::= n })");
  type::QualType qt = mod.context().qual_type(e);
  mod.compiler.CompleteWorkQueue();
  EXPECT_EQ(qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-constant-enumerator")));
}

// TODO: Handle situations where the type is specified in the declaration.

}  // namespace
}  // namespace compiler
