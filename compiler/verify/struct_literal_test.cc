#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/pointer.h"
#include "type/struct.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(StructLiteral, SuccessEmpty) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(struct {})");
  auto qts  = mod.context().qual_types(mod.get<ast::StructLiteral>());
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(StructLiteral, SuccessNonEmpty) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(struct {
    n: i64
    b := true
  }
  )");
  auto qts  = mod.context().qual_types(mod.get<ast::StructLiteral>());
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(StructLiteral, FieldError) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(struct {
    n: 3
    b := true
  })");
  auto qts  = mod.context().qual_types(mod.get<ast::StructLiteral>());
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

TEST(StructLiteral, SelfReferential) {
  test::CompilerInfrastructure infra;
  auto &mod             = infra.add_module(R"(
  list ::= struct {
    data: i64
    next: *list
  }
  l: list
  l
  )");
  auto qts              = mod.context().qual_types(mod.get<ast::Identifier>());
  type::Struct const *s = qts[0].type().if_as<type::Struct>();
  ASSERT_NE(s, nullptr);
  type::Struct::Field const *field = s->field("next");
  ASSERT_NE(field, nullptr);
  EXPECT_EQ(type::Type(type::Ptr(s)), field->type);
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(StructLiteral, DISABLED_SelfReferentialError) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  list ::= struct {
    data: i64
    next: list
  }
  l: list
  )");
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "incomplete-field")));
}

TEST(StructLiteral, MutuallyReferential) {
  test::CompilerInfrastructure infra;
  auto &mod  = infra.add_module(R"(
  A ::= struct { b_ptr: *B }
  B ::= struct { a_ptr: *A }
  a: A
  b: B
  a
  b
  )");
  auto stmts = mod.module().stmts();
  auto a_qts =
      mod.context().qual_types(&stmts[stmts.size() - 2]->as<ast::Identifier>());
  auto b_qts =
      mod.context().qual_types(&stmts[stmts.size() - 1]->as<ast::Identifier>());
  type::Struct const *a_struct = a_qts[0].type().if_as<type::Struct>();
  type::Struct const *b_struct = b_qts[0].type().if_as<type::Struct>();
  ASSERT_NE(a_struct, nullptr);
  ASSERT_NE(b_struct, nullptr);
  type::Struct::Field const *ab_field = a_struct->field("b_ptr");
  type::Struct::Field const *ba_field = b_struct->field("a_ptr");
  ASSERT_NE(ab_field, nullptr);
  ASSERT_NE(ba_field, nullptr);
  EXPECT_EQ(type::Type(type::Ptr(a_struct)), ba_field->type);
  EXPECT_EQ(type::Type(type::Ptr(b_struct)), ab_field->type);
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(StructLiteral, DISABLED_MutuallyReferentialError) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  A ::= struct { b: B }
  B ::= struct { a: A }
  )");
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "incomplete-field"),
                                   Pair("type-error", "incomplete-field")));
}
}  // namespace
}  // namespace compiler
