#include "compiler/compiler.h"
#include "compiler/module.h"
#include "frontend/source/buffer.h"
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

TEST(Access, EnumSuccess) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(E ::= enum { A \\ B \\ C })");
  auto const *enumerator = mod.Append<ast::Expression>(R"(E.A)");
  auto qts               = mod.context().qual_types(enumerator);
  ASSERT_THAT(qts, SizeIs(1));
  EXPECT_TRUE(qts[0].type().is<type::Enum>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, EnumMisnamed) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(E ::= enum { A \\ B \\ C })");
  auto const *enumerator = mod.Append<ast::Expression>(R"(E.D)");
  auto qts               = mod.context().qual_types(enumerator);
  EXPECT_TRUE(qts[0].type().is<type::Enum>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "missing-constant-member")));
}

TEST(Access, FlagsSuccess) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(F ::= flags { A \\ B \\ C })");
  auto const *flag = mod.Append<ast::Expression>(R"(F.A)");
  auto qts         = mod.context().qual_types(flag);
  EXPECT_TRUE(qts[0].type().is<type::Flags>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, FlagsMisnamed) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(F ::= flags { A \\ B \\ C })");
  auto const *flag = mod.Append<ast::Expression>(R"(F.D)");
  auto qts         = mod.context().qual_types(flag);
  EXPECT_TRUE(qts[0].type().is<type::Flags>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "missing-constant-member")));
}

TEST(Access, NonConstantType) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(T := i64)");
  auto const *expr = mod.Append<ast::Expression>(R"(T.something)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-constant-type-member-access")));
}

// TODO: Test covering an evaluation error when accessing a type member.

TEST(Access, TypeHasNoMembers) {
  test::TestModule mod;
  mod.Append<ast::Node>(R"(T ::= i64)");
  auto const *expr = mod.Append<ast::Expression>(R"(T.something)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "type-has-no-members")));
}

TEST(Access, AccessStructField) {
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: i64
    b: bool
  }
  non_constant: S
  constant :: S
  )");
  auto const *non_constant = mod.Append<ast::Expression>(R"(non_constant.n)");
  auto non_constant_qts    = mod.context().qual_types(non_constant);
  auto const *constant     = mod.Append<ast::Expression>(R"(constant.n)");
  auto constant_qts        = mod.context().qual_types(constant);
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  EXPECT_THAT(non_constant_qts,
              ElementsAre(type::QualType(type::I64, type::Quals::Ref())));
  EXPECT_THAT(constant_qts,
              UnorderedElementsAre(type::QualType(
                  type::I64, type::Quals::Ref() | type::Quals::Const())));
}

TEST(Access, NoFieldInStruct) {
  test::TestModule mod;
  mod.AppendCode(R"(
  S ::= struct {
    n: i64
    b: bool
  }
  s: S
  )");
  auto const *expr = mod.Append<ast::Expression>(R"(s.x)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, ConstantSliceLength) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"("abc".length)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType(
                       type::U64, type::Quals::Ref() | type::Quals::Const())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, NonConstantSliceLength) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr = mod.Append<ast::Expression>(R"(s.length)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType(type::U64, type::Quals::Ref())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, NonConstantSliceData) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr = mod.Append<ast::Expression>(R"(s.data)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType(type::BufPtr(type::Char),
                                              type::Quals::Ref())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, SliceInvalidMember) {
  test::TestModule mod;
  mod.AppendCode(R"(s := "abc")");
  auto const *expr = mod.Append<ast::Expression>(R"(s.size)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, ArrayLength) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([3; i64].length)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              ElementsAre(type::QualType::Constant(type::Array::LengthType())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, MultidimensionalArrayLength) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([3, 2; i64].length)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              ElementsAre(type::QualType::Constant(type::Array::LengthType())));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, ArrayElementType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([3, 2; i64].element_type)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Access, ArrayInvalidMember) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::Expression>(R"([3; i64].size)");
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "missing-constant-member")));
}

TEST(Access, IntoModuleWithError) {
  test::TestModule mod;
  CompiledModule imported_module;
  mod.CompileImportedLibrary(imported_module, "imported", R"(
  #{export} N :: bool = 3
  )");

  mod.AppendCode("mod ::= import \"imported\"");
  auto const *expr = mod.Append<ast::Expression>(R"(mod.N)");
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-cast")));
}


TEST(Access, Pattern) {
  test::TestModule mod;

  mod.AppendCode(R"(
  S ::= struct {
    n: i64
  }

  3 ~ (`s).n
  )");

  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("pattern-error", "deducing-access")));
}


// TODO: Field not exported from another module.
// TODO: Non-constant module
// TODO: Module evaluation failure
// TODO: Undeclared identifier across module boundaries
// TODO: Type error across module boundaries (other module already generated
// error)
// TODO: Valid access across module boundaries
// TODO: Valid overload set across module boundary
// TODO: Valid scope set across module boundary
// TODO: Valid mix of overloads and scopes across module boundary
// TODO: Invalid overload set across module boundaries
// TODO: Error on accessing incomplete member.

}  // namespace
}  // namespace compiler
