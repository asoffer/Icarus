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

TEST(Access, EnumSuccess) {
  test::CompilerInfrastructure infra;
  auto& mod =infra.add_module(R"(
  E ::= enum { A \\ B \\ C }

  E.A
  )");
  auto const *enumerator = mod.get<ast::Expression>();
  auto qts               = mod.context().qual_types(enumerator);
  ASSERT_THAT(qts, SizeIs(1));
  EXPECT_TRUE(qts[0].type().is<type::Enum>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, EnumMisnamed) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  E ::= enum { A \\ B \\ C }

  E.D
  )");
  auto const *enumerator = mod.get<ast::Expression>();
  auto qts               = mod.context().qual_types(enumerator);
  EXPECT_TRUE(qts[0].type().is<type::Enum>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "missing-constant-member")));
}

TEST(Access, FlagsSuccess) {
  test::CompilerInfrastructure infra;
  auto &mod              = infra.add_module(R"(
  F ::= flags { A \\ B \\ C }

  F.A
  )");
  auto const *flag = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(flag);
  EXPECT_TRUE(qts[0].type().is<type::Flags>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, FlagsMisnamed) {
  test::CompilerInfrastructure infra;
  auto &mod              = infra.add_module(R"(
  F ::= flags { A \\ B \\ C }

  F.D
  )");
  auto const *flag = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(flag);
  EXPECT_TRUE(qts[0].type().is<type::Flags>());
  EXPECT_EQ(qts[0].quals(), type::Quals::Const());
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "missing-constant-member")));
}

TEST(Access, NonConstantType) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  T := i64

  T.something)");

  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "non-constant-type-member-access")));
}

// TODO: Test covering an evaluation error when accessing a type member.

TEST(Access, TypeHasNoMembers) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  T ::= i64

  T.something)");

  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "type-has-no-members")));
}

TEST(Access, AccessStructField) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  S ::= struct {
    n: i64
    b: bool
  }
  non_constant: S
  constant :: S

  non_constant.n
  constant.n
  )");

  auto stmts               = mod.module().stmts();
  auto const *non_constant = &stmts[stmts.size() - 2]->as<ast::Expression>();
  auto const *constant     = &stmts[stmts.size() - 1]->as<ast::Expression>();
  auto non_constant_qts    = mod.context().qual_types(non_constant);
  auto constant_qts        = mod.context().qual_types(constant);
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
  EXPECT_THAT(non_constant_qts,
              ElementsAre(type::QualType(type::I64, type::Quals::Ref())));
  EXPECT_THAT(constant_qts,
              UnorderedElementsAre(type::QualType(
                  type::I64, type::Quals::Ref() | type::Quals::Const())));
}

TEST(Access, NoFieldInStruct) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  S ::= struct {
    n: i64
    b: bool
  }
  s: S
  )");
  auto const *expr = infra.add_module(R"(s.x)").get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, ConstantSliceLength) {
  test::CompilerInfrastructure infra;
  auto& mod = infra.add_module(R"("abc".length)");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType(
                       type::U64, type::Quals::Ref() | type::Quals::Const())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, NonConstantSliceLength) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  s := "abc"

  s.length)");

  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType(type::U64, type::Quals::Ref())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, NonConstantSliceData) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  s := "abc"

  s.data)");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType(type::BufPtr(type::Char),
                                              type::Quals::Ref())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, SliceInvalidMember) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  s := "abc"

  s.size)");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, ArrayLength) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([3; i64].length)");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              ElementsAre(type::QualType::Constant(type::Array::LengthType())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, MultidimensionalArrayLength) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([3, 2; i64].length)");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              ElementsAre(type::QualType::Constant(type::Array::LengthType())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, ArrayElementType) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([3, 2; i64].element_type)");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, ArrayInvalidMember) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"([3; i64].size)");
  auto const *expr = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, ElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "missing-constant-member")));
}

TEST(Access, IntoModuleWithError) {
  test::CompilerInfrastructure infra;
  infra.add_module("imported", R"(
  #{export} N :: bool = 3
  )");

  auto &mod        = infra.add_module("mod ::= import \"imported\"");
  auto const *expr = infra.add_module(R"(mod.N)").get<ast::Expression>();
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-cast")));
}

TEST(Access, CrossModuleStructFieldAccess) {
  test::CompilerInfrastructure infra;
  infra.add_module("imported", R"(
  #{export} S ::= struct {
    #{export} n: i64
  }
  )");

  auto &mod = infra.add_module(R"(
    mod ::= import "imported"
    s: mod.S
    s.n
  )");

  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Access, CrossModuleStructFieldError) {
  test::CompilerInfrastructure infra;
  infra.add_module("imported", R"(
  #{export} S ::= struct {
    #{export} n: i64
  }
  )");

  auto &mod = infra.add_module(R"(
    mod ::= import "imported"
    s: mod.S
    s.m
  )");

  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "missing-member")));
}

TEST(Access, Pattern) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  S ::= struct {
    n: i64
  }

  3 ~ (`s).n
  )");

  EXPECT_THAT(infra.diagnostics(),
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
