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

TEST(Copy, Success) {
  {
    test::TestModule mod;
    auto const *expr = mod.Append<ast::UnaryOperator>("copy [1, 2, 3]");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(3, type::Int64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    a := [1, 2, 3]
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("copy a");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Arr(3, type::Int64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Init, Success) {
  {
    test::TestModule mod;
    auto const *expr = mod.Append<ast::UnaryOperator>("init [1, 2, 3]");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(3, type::Int64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    a := [1, 2, 3]
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("init a");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Arr(3, type::Int64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Copy, Uncopyable) {
  test::TestModule mod;
  mod.AppendCode(R"(
  T ::= #{uncopyable} struct {}
  t: T
  )");
  auto const *id      = mod.Append<ast::Identifier>("t");
  auto const *id_qt   = mod.context().qual_type(id);
  auto const *expr    = mod.Append<ast::UnaryOperator>("copy t");
  auto const *expr_qt = mod.context().qual_type(expr);
  ASSERT_NE(expr_qt, nullptr);
  ASSERT_NE(id_qt, nullptr);
  EXPECT_EQ(*expr_qt, type::QualType(id_qt->type(),
                                     id_qt->quals() & ~type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "uncopyable-type")));
}

TEST(Move, Success) {
  {
    test::TestModule mod;
    auto const *expr = mod.Append<ast::UnaryOperator>("move [1, 2, 3]");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(3, type::Int64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    a := [1, 2, 3]
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("move a");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Arr(3, type::Int64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Move, Immovable) {
  test::TestModule mod;
  mod.AppendCode(R"(
  T ::= #{immovable} struct {}
  t: T
  )");
  auto const *id      = mod.Append<ast::Identifier>("t");
  auto const *id_qt   = mod.context().qual_type(id);
  auto const *expr    = mod.Append<ast::UnaryOperator>("move t");
  auto const *expr_qt = mod.context().qual_type(expr);
  ASSERT_NE(expr_qt, nullptr);
  ASSERT_NE(id_qt, nullptr);
  EXPECT_EQ(*expr_qt, type::QualType(id_qt->type(),
                                     id_qt->quals() & ~type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "immovable-type")));
}

TEST(BufferPointer, Success) {
  {
    test::TestModule mod;
    auto const *expr = mod.Append<ast::UnaryOperator>("[*]int64");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := int64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("[*]T");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Type_));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(BufferPointer, NonType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::UnaryOperator>("[*]17");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

TEST(TypeOf, Success) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::UnaryOperator>("3:?");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Eval, Success) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::UnaryOperator>("`3");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Int64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Eval, NonConstant) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n := 3
  )");
  auto const *expr = mod.Append<ast::UnaryOperator>("`n");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Int64));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("evaluation-error", "non-constant-evaluation")));
}

TEST(At, Pointer) {
  test::TestModule mod;
  mod.AppendCode(R"(
  p: *int64
  )");
  auto const *expr = mod.Append<ast::UnaryOperator>("@p");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(At, BufferPointer) {
  test::TestModule mod;
  mod.AppendCode(R"(
  p: [*]int64
  )");
  auto const *expr = mod.Append<ast::UnaryOperator>("@p");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType(type::Int64, type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(At, NonPointer) {
  test::TestModule mod;
  mod.AppendCode(R"(
  p: int64
  )");
  auto const *expr = mod.Append<ast::UnaryOperator>("@p");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "dereferencing-non-pointer")));
}

TEST(And, Success) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n: int64
  )");
  auto const *expr = mod.Append<ast::UnaryOperator>("&n");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Ptr(type::Int64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(And, NonReference) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::UnaryOperator>("&3");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "non-addressable-expression")));
}

TEST(Pointer, Success) {
  {
    test::TestModule mod;
    auto const *expr = mod.Append<ast::UnaryOperator>("*int64");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := int64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("*T");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Type_));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Pointer, NotAType) {
  test::TestModule mod;
  auto const *expr = mod.Append<ast::UnaryOperator>("*3");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

TEST(Negate, SignedInteger) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: int64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("-n");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: int64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("-n");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Negate, FloatingPoint) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    x: float64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("-x");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Float64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    x :: float64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("-x");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Float64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Negate, UnsignedInteger) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: nat64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("-n");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_EQ(qt, nullptr);
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "negating-unsigned-integer")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: nat64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("-n");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_EQ(qt, nullptr);
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "negating-unsigned-integer")));
  }
}

TEST(Negate, InvalidType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n := [1, 2, 3]
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("-n");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_EQ(qt, nullptr);
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n ::= [1, 2, 3]
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("-n");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_EQ(qt, nullptr);
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }
}

TEST(Negate, Overload) {
  test::TestModule mod;
  mod.AppendCode(R"(
    S ::= struct {}
    (-) ::= (s: S) -> int64 { return 0 }
    )");
  auto const *expr = mod.Append<ast::UnaryOperator>("-S.{}");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Int64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Negate, MissingOverload) {
  test::TestModule mod;
  mod.AppendCode(R"(
    S ::= struct {}
    )");
  auto const *expr = mod.Append<ast::UnaryOperator>("-S.{}");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-unary-operator-overload")));
}

TEST(Not, Bool) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    b: bool
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("!b");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    b :: bool
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("!b");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Bool));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Not, Flags) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    F ::= flags { A \\ B \\ C }
    f: F
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("!f");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_TRUE(qt->type().is<type::Flags>());
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    F ::= flags { A \\ B \\ C }
    f :: F
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("!f");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_NE(qt, nullptr);
    EXPECT_TRUE(qt->type().is<type::Flags>());
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Not, InvalidType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: int64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("!n");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_EQ(qt, nullptr);
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: int64
    )");
    auto const *expr = mod.Append<ast::UnaryOperator>("!n");
    auto const *qt   = mod.context().qual_type(expr);
    ASSERT_EQ(qt, nullptr);
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }
}

TEST(Not, Overload) {
  test::TestModule mod;
  mod.AppendCode(R"(
    S ::= struct {}
    (!) ::= (s: S) -> int64 { return 0 }
    )");
  auto const *expr = mod.Append<ast::UnaryOperator>("!S.{}");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_NE(qt, nullptr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Int64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Not, MissingOverload) {
  test::TestModule mod;
  mod.AppendCode(R"(
    S ::= struct {}
    )");
  auto const *expr = mod.Append<ast::UnaryOperator>("!S.{}");
  auto const *qt   = mod.context().qual_type(expr);
  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-unary-operator-overload")));
}

}  // namespace
}  // namespace compiler
