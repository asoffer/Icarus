#include "compiler/compiler.h"
#include "compiler/module.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(DesignatedInitializer, NonConstantType) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  S := struct {
    n: i64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{})");
  auto qts         = mod.context().qual_types(expr);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair(
                  "type-error", "non-constant-designated-initializer-type")));
}

TEST(DesignatedInitializer, NonType) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  NotAType ::= 3
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(NotAType.{})");
  auto qts         = mod.context().qual_types(expr);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-type-designated-initializer-type")));
}

TEST(DesignatedInitializer, NonConstantNonType) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  NotAType: i64
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(NotAType.{})");
  auto qts         = mod.context().qual_types(expr);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(
          Pair("type-error", "non-constant-designated-initializer-type"),
          Pair("type-error", "non-type-designated-initializer-type")));
}

// TODO: Evaluation error test

TEST(DesignatedInitializer, NonStrucType) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  NotAStruct ::= i64
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(NotAStruct.{})");
  auto qts         = mod.context().qual_types(expr);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair(
                  "type-error", "non-struct-designated-initializer-type")));
}

TEST(DesignatedInitializer, FieldErrorsAndStructErrors) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  NotAStruct ::= i64
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(NotAStruct.{
    field = NotAStruct.{}
  }
  )");
  auto qts         = mod.context().qual_types(expr);
  ASSERT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(
          Pair("type-error", "non-struct-designated-initializer-type"),
          Pair("type-error", "non-struct-designated-initializer-type")));
}

TEST(DesignatedInitializer, NonMember) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  S ::= struct {
    n: i64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    not_a_field = 0
  }
  )");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Unqualified());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-struct-field")));
}

TEST(DesignatedInitializer, MemberInvalidConversion) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  S ::= struct {
    n: i64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    n = "abc"
  }
  )");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Unqualified());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "invalid-initializer-type")));
}

TEST(DesignatedInitializer, Valid) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  S ::= struct {
    n: i64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    n = 0
  }
  )");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, MultipleMemberAssignments) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  f ::= () -> (i64, bool) { return 3, true }
  S ::= struct {
    n: i64
    b: bool
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    (n, b) = f()
  }
  )");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Unqualified());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, MultipleMemberInvalidAssignments) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  f ::= () -> (i64, i64) { return 3, 4 }
  S ::= struct {
    n: i64
    b: bool
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    (n, b) = f()
  }
  )");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Unqualified());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "invalid-initializer-type")));
}

TEST(DesignatedInitializer, MemberValidConversion) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  buffer_pointer: [*]i64

  S ::= struct {
    pointer: *i64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    pointer = buffer_pointer
  }
  )");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Unqualified());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, ErrorInInitializerAndField) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  NotAType ::= 0
  S ::= struct {
    n: i64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    m = NotAType.{}  // Still generate missing-struct-field for `m`.
  }
  )");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Unqualified());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-type-designated-initializer-type"),
                  Pair("type-error", "missing-struct-field")));
}

TEST(DesignatedInitializer, CrossModule) {
  test::CompilerInfrastructure infra;
  auto &imported_mod = infra.add_module("imported", R"(
  #{export} S ::= struct {
    #{export} n: i64
  }
  )");
  auto &mod = infra.add_module(R"(-- ::= import "imported")");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{ n = 3 })");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, NotExported) {
  test::CompilerInfrastructure infra;
  auto &imported_mod = infra.add_module("imported", R"(
  #{export} S ::= struct {
    n: i64
  }
  )");
  auto &mod = infra.add_module(R"(-- ::= import "imported")");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{ n = 3 })");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "non-exported-field")));
}

}  // namespace
}  // namespace compiler
