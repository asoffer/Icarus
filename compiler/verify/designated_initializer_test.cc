#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "frontend/source/buffer.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::Eq;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(DesignatedInitializer, NonConstantType) {
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, MultipleMemberAssignments) {
  // Verify that errors on fields are emit even when there's an error on the
  // struct type.
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  test::TestModule mod;
  mod.AppendCode(R"(
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
  auto id = ir::ModuleId::New();
  LibraryModule imported_mod;
  imported_mod.set_diagnostic_consumer<diagnostic::TrackingConsumer>();

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported")))
      .WillByDefault([id](std::string_view) { return id; });
  ON_CALL(mod.importer, get(id))
      .WillByDefault(
          [&](ir::ModuleId) -> module::BasicModule & { return imported_mod; });

  frontend::SourceBuffer buffer(R"(
  #{export} S ::= struct {
    #{export} n: i64
  }
  )");
  imported_mod.AppendNodes(frontend::Parse(buffer, mod.consumer),
                                    mod.consumer, mod.importer);

  mod.AppendCode("-- ::= import \"imported\"");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{ n = 3 })");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(DesignatedInitializer, NotExported) {
  auto id = ir::ModuleId::New();
  LibraryModule imported_mod;
  imported_mod.set_diagnostic_consumer<diagnostic::TrackingConsumer>();

  test::TestModule mod;
  ON_CALL(mod.importer, Import(Eq("imported")))
      .WillByDefault([id](std::string_view) { return id; });
  ON_CALL(mod.importer, get(id))
      .WillByDefault(
          [&](ir::ModuleId) -> module::BasicModule & { return imported_mod; });

  frontend::SourceBuffer buffer(R"(
  #{export} S ::= struct {
    n: i64
  }
  )");
  imported_mod.AppendNodes(frontend::Parse(buffer, mod.consumer), mod.consumer,
                           mod.importer);

  mod.AppendCode("-- ::= import \"imported\"");
  auto const *expr = mod.Append<ast::Expression>(R"(S.{ n = 3 })");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_TRUE(qts[0].type().is<type::Struct>());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "non-exported-field")));
}

}  // namespace
}  // namespace compiler
