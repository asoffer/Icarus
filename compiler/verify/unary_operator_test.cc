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
    auto const *expr  = mod.Append<ast::UnaryOperator>("copy [1, 2, 3]");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(3, type::I64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    a := [1, 2, 3]
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("copy a");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Arr(3, type::I64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Init, Success) {
  {
    test::TestModule mod;
    auto const *expr  = mod.Append<ast::UnaryOperator>("init [1, 2, 3]");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(3, type::I64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    a := [1, 2, 3]
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("init a");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Arr(3, type::I64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Copy, Uncopyable) {
  test::TestModule mod;
  mod.AppendCode(R"(
  T ::= #{uncopyable} struct {}
  t: T
  )");
  auto const *id         = mod.Append<ast::Identifier>("t");
  type::QualType id_qt   = mod.context().qual_type(id);
  auto const *expr       = mod.Append<ast::UnaryOperator>("copy t");
  type::QualType expr_qt = mod.context().qual_type(expr);
  EXPECT_EQ(*expr_qt,
            type::QualType(id_qt.type(), id_qt.quals() & ~type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "uncopyable-type")));
}

TEST(Move, Success) {
  {
    test::TestModule mod;
    auto const *expr  = mod.Append<ast::UnaryOperator>("move [1, 2, 3]");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(3, type::I64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    a := [1, 2, 3]
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("move a");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Arr(3, type::I64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Move, Immovable) {
  test::TestModule mod;
  mod.AppendCode(R"(
  T ::= #{immovable} struct {}
  t: T
  )");
  auto const *id         = mod.Append<ast::Identifier>("t");
  type::QualType id_qt   = mod.context().qual_type(id);
  auto const *expr       = mod.Append<ast::UnaryOperator>("move t");
  type::QualType expr_qt = mod.context().qual_type(expr);
  EXPECT_EQ(expr_qt,
            type::QualType(id_qt.type(), id_qt.quals() & ~type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "immovable-type")));
}

TEST(BufferPointer, Success) {
  {
    test::TestModule mod;
    auto const *expr  = mod.Append<ast::UnaryOperator>("[*]i64");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := i64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("[*]T");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Type_));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(BufferPointer, NonType) {
  test::TestModule mod;
  auto const *expr  = mod.Append<ast::UnaryOperator>("[*]17");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

TEST(TypeOf, Success) {
  test::TestModule mod;
  auto const *expr  = mod.Append<ast::UnaryOperator>("3:?");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(At, Pointer) {
  test::TestModule mod;
  mod.AppendCode(R"(
  p: *i64
  )");
  auto const *expr  = mod.Append<ast::UnaryOperator>("@p");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(*qt, type::QualType(type::I64, type::Quals::Ref()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(At, BufferPointer) {
  test::TestModule mod;
  mod.AppendCode(R"(
  p: [*]i64
  )");
  auto const *expr  = mod.Append<ast::UnaryOperator>("@p");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(*qt, type::QualType(type::I64, type::Quals::Buf()));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(At, NonPointer) {
  test::TestModule mod;
  mod.AppendCode(R"(
  p: i64
  )");
  auto const *expr  = mod.Append<ast::UnaryOperator>("@p");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "dereferencing-non-pointer")));
}

TEST(And, Success) {
  test::TestModule mod;
  mod.AppendCode(R"(
  n: i64
  )");
  auto const *expr  = mod.Append<ast::UnaryOperator>("&n");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::Ptr(type::I64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(And, NonReference) {
  test::TestModule mod;
  auto const *expr  = mod.Append<ast::UnaryOperator>("&3");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "non-addressable-expression")));
}

TEST(Pointer, Success) {
  {
    test::TestModule mod;
    auto const *expr  = mod.Append<ast::UnaryOperator>("*i64");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Type_));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := i64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("*T");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Type_));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Pointer, NotAType) {
  test::TestModule mod;
  auto const *expr  = mod.Append<ast::UnaryOperator>("*3");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "not-a-type")));
}

TEST(Negate, SignedInteger) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: i64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("-n");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::I64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: i64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("-n");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::I64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Negate, FloatingPoint) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    x: f64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("-x");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::F64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    x :: f64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("-x");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::F64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Negate, UnsignedInteger) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: u64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("-n");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(qt, type::QualType::Error());
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "negating-unsigned-integer")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: u64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("-n");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(qt, type::QualType::Error());
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
    auto const *expr  = mod.Append<ast::UnaryOperator>("-n");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(qt, type::QualType::Error());
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n ::= [1, 2, 3]
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("-n");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(qt, type::QualType::Error());
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }
}

TEST(Negate, Overload) {
  test::TestModule mod;
  mod.AppendCode(R"(
    S ::= struct {}
    (-) ::= (s: S) -> i64 { return 0 }
    )");
  auto const *expr  = mod.Append<ast::UnaryOperator>("-S.{}");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::I64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Negate, MissingOverload) {
  test::TestModule mod;
  mod.AppendCode(R"(
    S ::= struct {}
    )");
  auto const *expr  = mod.Append<ast::UnaryOperator>("-S.{}");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
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
    auto const *expr  = mod.Append<ast::UnaryOperator>("!b");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Bool));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    b :: bool
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("!b");
    type::QualType qt = mod.context().qual_type(expr);
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
    auto const *expr  = mod.Append<ast::UnaryOperator>("!f");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_TRUE(qt.type().is<type::Flags>());
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    F ::= flags { A \\ B \\ C }
    f :: F
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("!f");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_TRUE(qt.type().is<type::Flags>());
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Not, InvalidType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: i64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("!n");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(qt, type::QualType::Error());
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: i64
    )");
    auto const *expr  = mod.Append<ast::UnaryOperator>("!n");
    type::QualType qt = mod.context().qual_type(expr);
    EXPECT_EQ(qt, type::QualType::Error());
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "invalid-unary-operator-call")));
  }
}

TEST(Not, Overload) {
  test::TestModule mod;
  mod.AppendCode(R"(
    S ::= struct {}
    (!) ::= (s: S) -> i64 { return 0 }
    )");
  auto const *expr  = mod.Append<ast::UnaryOperator>("!S.{}");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(*qt, type::QualType::NonConstant(type::I64));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Not, MissingOverload) {
  test::TestModule mod;
  mod.AppendCode(R"(
    S ::= struct {}
    )");
  auto const *expr  = mod.Append<ast::UnaryOperator>("!S.{}");
  type::QualType qt = mod.context().qual_type(expr);
  EXPECT_EQ(qt, type::QualType::Error());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-unary-operator-overload")));
}

}  // namespace
}  // namespace compiler
