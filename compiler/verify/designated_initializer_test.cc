#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(DesignatedInitializer, NonConstantType) {
  test::TestModule mod;
  mod.AppendCode(R"(
  S := struct {
    n: int64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{})");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair(
                  "type-error", "non-constant-designated-initializer-type")));
}

TEST(DesignatedInitializer, NonType) {
  test::TestModule mod;
  mod.AppendCode(R"(
  NotAType ::= 3
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(NotAType.{})");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-type-designated-initializer-type")));
}

TEST(DesignatedInitializer, NonConstantNonType) {
  test::TestModule mod;
  mod.AppendCode(R"(
  NotAType: int64
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(NotAType.{})");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(
          Pair("type-error", "non-constant-designated-initializer-type"),
          Pair("type-error", "non-type-designated-initializer-type")));
}

// TODO: Evaluation error test

TEST(DesignatedInitializer, NonStrucType) {
  test::TestModule mod;
  mod.AppendCode(R"(
  NotAStruct ::= int64
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(NotAStruct.{})");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair(
                  "type-error", "non-struct-designated-initializer-type")));
}

TEST(DesignatedInitializer, FieldErrorsAndStructErrors) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
  NotAStruct ::= int64
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(NotAStruct.{
    field = NotAStruct.{}
  }
  )");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(
          Pair("type-error", "non-struct-designated-initializer-type"),
          Pair("type-error", "non-struct-designated-initializer-type")));
}

TEST(DesignatedInitializer, NonMember) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: int64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    not_a_field = 0
  }
  )");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type().is<type::Struct>());
  EXPECT_EQ(qt->quals(), type::Quals::Unqualified());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-struct-field")));
}

TEST(DesignatedInitializer, MemberInvalidConversion) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: int64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    n = "abc"
  }
  )");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type().is<type::Struct>());
  EXPECT_EQ(qt->quals(), type::Quals::Unqualified());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "invalid-initializer-type")));
}

TEST(DesignatedInitializer, Valid) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: int64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    n = 0
  }
  )");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type().is<type::Struct>());
  EXPECT_EQ(qt->quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, MultipleMemberAssignments) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
  f ::= () -> (int64, bool) { return 3, true }
  S ::= struct {
    n: int64
    b: bool
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    (n, b) = f()
  }
  )");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type().is<type::Struct>());
  EXPECT_EQ(qt->quals(), type::Quals::Unqualified());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, MultipleMemberInvalidAssignments) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
  f ::= () -> (int64, int64) { return 3, 4 }
  S ::= struct {
    n: int64
    b: bool
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    (n, b) = f()
  }
  )");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type().is<type::Struct>());
  EXPECT_EQ(qt->quals(), type::Quals::Unqualified());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "invalid-initializer-type")));
}

TEST(DesignatedInitializer, MemberValidConversion) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
  buffer_pointer: [*]int64

  S ::= struct {
    pointer: *int64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    pointer = buffer_pointer
  }
  )");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type().is<type::Struct>());
  EXPECT_EQ(qt->quals(), type::Quals::Unqualified());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, ErrorInInitializerAndField) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
  NotAType ::= 0
  S ::= struct {
    n: int64
  }
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{
    m = NotAType.{}  // Still generate missing-struct-field for `m`.
  }
  )");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_TRUE(qt->type().is<type::Struct>());
  EXPECT_EQ(qt->quals(), type::Quals::Unqualified());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-type-designated-initializer-type"),
                  Pair("type-error", "missing-struct-field")));
}

}  // namespace
}  // namespace compiler
