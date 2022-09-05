#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "ir/value/slice.h"
#include "semantic_analysis/type_verification/matchers.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"

namespace semantic_analysis {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Pointee;
using ::testing::UnorderedElementsAre;

TEST(BufferPointer, Success) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"([*]i64)");
  EXPECT_THAT(infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
              ElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(BufferPointer, NonType) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"([*]17)");
  EXPECT_THAT(infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
              ElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

TEST(TypeOf, Success) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"(3:?)");
  EXPECT_THAT(infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
              ElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(At, Pointer) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"(
  p: *i64
  @p
  )");

  EXPECT_THAT(
      infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
      ElementsAre(type::QualType(type::I64, type::Qualifiers::Storage())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(At, BufferPointer) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"(
  p: [*]i64
  @p
  )");

  EXPECT_THAT(
      infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
      ElementsAre(type::QualType(type::I64, type::Qualifiers::Buffer())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(At, NonPointer) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"(
  p: i64
  @p
  )");

  EXPECT_THAT(infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
              ElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "dereferencing-non-pointer")));
}

TEST(Address, Success) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"(
  n: i64
  &n
  )");
  EXPECT_THAT(infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
              ElementsAre(type::QualType::NonConstant(type::Ptr(type::I64))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Address, NonReference) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"(&3)");
  EXPECT_THAT(infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
              ElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "non-addressable-expression")));
}

TEST(Pointer, Success) {
  {
    Infrastructure infra;
    auto nodes = infra.ParseAndVerify(R"(*i64)");
    EXPECT_THAT(
        infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
        ElementsAre(type::QualType::Constant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

#if 0
  {
    Infrastructure infra;
    auto nodes = infra.ParseAndVerify(R"(
    T := i64
    *T
    )");
    EXPECT_THAT(
        infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
        ElementsAre(type::QualType::NonConstant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
#endif
}

TEST(Pointer, NotAType) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(R"(*3)");
  EXPECT_THAT(infra.context().qual_types(&nodes.back()->as<ast::Expression>()),
              ElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

#if 0
TEST(Negate, SignedInteger) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n: i64
    -n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::NonConstant(type::I64)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n :: i64
    -n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::I64)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Negate, FloatingPoint) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    x: f64
    -x
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::NonConstant(type::F64)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    x :: f64
    -x
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::F64)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Negate, UnsignedInteger) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n: u64
    -n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "negating-unsigned-integer")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n :: u64
    -n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "negating-unsigned-integer")));
  }
}

TEST(Negate, InvalidType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n := [1, 2, 3]
    -n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n ::= [1, 2, 3]
    -n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }
}

TEST(Negate, Overload) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
    S ::= struct {}
    (-) ::= (s: S) -> i64 { return 0 }
    -S.{}
  )");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::NonConstant(type::I64)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Negate, MissingOverload) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
    S ::= struct {}
    -S.{}
  )");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-unary-operator-overload")));
}

TEST(Not, Bool) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    b: bool
    not b
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::NonConstant(type::Bool)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    b :: bool
    not b
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::Constant(type::Bool)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Not, Flags) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    F ::= flags { A \\ B \\ C }
    f: F
    not f
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_TRUE(qts[0].type().is<type::Flags>());
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    F ::= flags { A \\ B \\ C }
    f :: F
    not f
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_TRUE(qts[0].type().is<type::Flags>());
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Not, InvalidType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n: i64
    not n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n: i64
    not n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n :: i64
    not n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    n :: i64
    not n
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }
}

TEST(Unexpanded, Failure) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
    f ::= () -> (i64, i64) { return 1, 2 }
    g ::= () -> i64 { return 1 }
    h ::= () -> () { }
    -'f
    -'g
    -'h
  )");
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "unexpanded-unary-operator-argument"),
                  Pair("type-error", "unexpanded-unary-operator-argument")));
}
#endif

}  // namespace
}  // namespace semantic_analysis
