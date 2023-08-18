#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;
using ::testing::Pair;

TEST(BufferPointer, Success) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"([*]i64)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
}

TEST(BufferPointer, NonType) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"([*]17)"),
              AllOf(HasQualTypes(Error(Constant(Type))),
                    HasDiagnostics(Pair("type-error", "not-a-type"))));
}

TEST(TypeOf, Success) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(3:?)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));

  EXPECT_THAT(repl.type_check(R"(
  n: i32
  n:?
  )"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
}

TEST(At, Pointer) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  p: *i64
  @p
  )"),
              AllOf(HasQualTypes(Reference(I(64))), HasDiagnostics()));
}

TEST(At, BufferPointer) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  p: [*]i64
  @p
  )"),
              AllOf(HasQualTypes(QualifiedType(I(64), Qualifiers::Buffer())),
                    HasDiagnostics()));
}

TEST(At, NonPointer) {
  test::Repl repl;
  EXPECT_THAT(
      repl.type_check(R"(
  p: i64
  @p
  )"),
      AllOf(HasQualTypes(Error()),
            HasDiagnostics(Pair("type-error", "dereferencing-non-pointer"))));
}

TEST(Address, Success) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
      n: i64
      &n
      )"),
              AllOf(HasQualTypes(QualifiedType(
                        core::PointerType(GlobalTypeSystem, I(64)))),
                    HasDiagnostics()));
}

TEST(Address, NonReference) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(&3)"),
              AllOf(HasQualTypes(Error()),
                    HasDiagnostics(Pair("value-category-error",
                                        "non-addressable-expression"))));
}

TEST(Pointer, Success) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(*i64)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));

  EXPECT_THAT(repl.type_check(R"(
  T := i64
  *T
  )"),
              AllOf(HasQualTypes(QualifiedType(Type)), HasDiagnostics()));
}

TEST(Pointer, NotAType) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(*3)"),
              AllOf(HasQualTypes(Error(Constant(Type))),
                    HasDiagnostics(Pair("type-error", "not-a-type"))));
}

TEST(Negation, SignedInteger) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  n: i64
  -n
  )"),
              AllOf(HasQualTypes(QualifiedType(I(64))), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(
  m :: i64
  -m
  )"),
              AllOf(HasQualTypes(Constant(I(64))), HasDiagnostics()));
}

TEST(Negation, FloatingPoint) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  x: f32
  -x
  )"),
              AllOf(HasQualTypes(QualifiedType(F32)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(
  y :: f32
  -y
  )"),
              AllOf(HasQualTypes(Constant(F32)), HasDiagnostics()));
}

TEST(Negation, UnsignedInteger) {
  {
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    j: u64
    -j
    )"),
        AllOf(HasQualTypes(Error()),
              HasDiagnostics(Pair("type-error", "negating-unsigned-integer"))));
  }
  {
    test::Repl repl;
    EXPECT_THAT(
        repl.type_check(R"(
    k :: u64
    -k
    )"),
        AllOf(HasQualTypes(Constant(Error())),
              HasDiagnostics(Pair("type-error", "negating-unsigned-integer"))));
  }
}

TEST(Negation, InvalidType) {
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    b := true
    -b
    )"),
                AllOf(HasQualTypes(Error()),
                      HasDiagnostics(
                          Pair("type-error", "invalid-unary-operator-call"))));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    a ::= [1, 2, 3]
    -a
    )"),
                AllOf(HasQualTypes(Constant(Error())),
                      HasDiagnostics(
                          Pair("type-error", "invalid-unary-operator-call"))));
  }
}

#if 0
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
              UnorderedElementsAre(NonConstant(I(64))));
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
  EXPECT_THAT(qts, UnorderedElementsAre(Error()));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-unary-operator-overload")));
}
#endif

TEST(Not, Bool) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(
  b: bool
  not b
  )"),
              AllOf(HasQualTypes(QualifiedType(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(
  c :: bool
  not c
  )"),
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));
}

#if 0
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
#endif

TEST(Not, InvalidType) {
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    m: i32
    not m
    )"),
                AllOf(HasQualTypes(Error()),
                      HasDiagnostics(
                          Pair("type-error", "invalid-unary-operator-call"))));
  }
  {
    test::Repl repl;
    EXPECT_THAT(repl.type_check(R"(
    n :: i32
    not n
    )"),
                AllOf(HasQualTypes(Constant(Error())),
                      HasDiagnostics(
                          Pair("type-error", "invalid-unary-operator-call"))));
  }
}

#if 0
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
