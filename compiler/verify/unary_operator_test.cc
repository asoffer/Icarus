#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/array.h"
#include "type/pointer.h"
#include "type/primitive.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

// TODO: Remove the need to cast array literal elements from constants.

TEST(Copy, Success) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module("copy [1 as i64, 2 as i64, 3 as i64]");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(3, type::I64))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    a := [1 as i64, 2 as i64, 3 as i64]
    copy a
    )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::NonConstant(type::Arr(3, type::I64))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Init, Success) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module("init [1, 2, 3]");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(
                         type::Arr(3, type::Integer))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module("init [1 as i64, 2 as i64, 3 as i64]");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(3, type::I64))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    a := [1 as i64, 2 as i64, 3 as i64]
    init a
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::NonConstant(type::Arr(3, type::I64))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Copy, Uncopyable) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  T ::= #{uncopyable} struct {}
  t: T
  t
  copy t
  )");
  auto stmts       = mod.module().stmts();
  auto const *id   = &stmts[stmts.size() - 2]->as<ast::Identifier>();
  auto id_qts      = mod.context().qual_types(id);
  auto const *expr = &stmts[stmts.size() - 1]->as<ast::UnaryOperator>();
  auto expr_qts    = mod.context().qual_types(expr);
  EXPECT_THAT(expr_qts,
              UnorderedElementsAre(type::QualType(
                  id_qts[0].type(), id_qts[0].quals() & ~type::Quals::Ref())));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "uncopyable-type")));
}

TEST(Move, Success) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module("move [1 as i64, 2 as i64, 3 as i64]");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::Constant(type::Arr(3, type::I64))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    a := [1 as i64, 2 as i64, 3 as i64]
    move a
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts, UnorderedElementsAre(
                         type::QualType::NonConstant(type::Arr(3, type::I64))));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Move, Immovable) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  T ::= #{immovable} struct {}
  t: T
  t
  move t
  )");
  auto stmts       = mod.module().stmts();
  auto const *id   = &stmts[stmts.size() - 2]->as<ast::Identifier>();
  auto id_qts      = mod.context().qual_types(id);
  auto const *expr = &stmts[stmts.size() - 1]->as<ast::UnaryOperator>();
  auto expr_qts    = mod.context().qual_types(expr);
  EXPECT_THAT(expr_qts,
              UnorderedElementsAre(type::QualType(
                  id_qts[0].type(), id_qts[0].quals() & ~type::Quals::Ref())));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "immovable-type")));
}

TEST(BufferPointer, Success) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module("[*]i64");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::Constant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    T := i64
    [*]T
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::NonConstant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(BufferPointer, NonType) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module("[*]17");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

TEST(TypeOf, Success) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module("3:?");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Constant(type::Type_)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(At, Pointer) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  p: *i64
  @p
  )");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType(type::I64, type::Quals::Ref())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(At, BufferPointer) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  p: [*]i64
  @p
  )");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(
      qts, UnorderedElementsAre(type::QualType(type::I64, type::Quals::Buf())));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(At, NonPointer) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  p: i64
  @p
  )");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "dereferencing-non-pointer")));
}

TEST(And, Success) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(
  n: i64
  &n
  )");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(
                       type::QualType::NonConstant(type::Ptr(type::I64))));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(And, NonReference) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module("&3");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "non-addressable-expression")));
}

TEST(Pointer, Success) {
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module("*i64");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::Constant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod        = infra.add_module(R"(
    T := i64
    *T
  )");
    auto const *expr = mod.get<ast::UnaryOperator>();
    auto qts         = mod.context().qual_types(expr);
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType::NonConstant(type::Type_)));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Pointer, NotAType) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module("*3");
  auto const *expr = mod.get<ast::UnaryOperator>();
  auto qts         = mod.context().qual_types(expr);
  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

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

TEST(UnaryOperator, ValidPattern) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  true ~ not `B
  )");
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(UnaryOperator, InvalidPattern) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
  true ~ -`B
  )");
  EXPECT_THAT(
      infra.diagnostics(),
      UnorderedElementsAre(Pair("pattern-error", "pattern-type-mismatch")));
}

}  // namespace
}  // namespace compiler
