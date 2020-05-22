#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(Declaration, DefaultInitSuccess) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: int64
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: int64
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, DefaultInitTypeNotAType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: 3 
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: 3 
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }
}

TEST(Declaration, DefaultInitNonConstantType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := int64
    n: T 
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := int64
    n :: T
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }
}

TEST(Declaration, DefaultInitNonDefaultable) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: int64 | bool
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(
        *qt, type::QualType::NonConstant(type::Var({type::Int64, type::Bool})));
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "no-default-value")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: int64 | bool
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt,
              type::QualType::Constant(type::Var({type::Int64, type::Bool})));
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "no-default-value")));
  }
}

TEST(Declaration, InferredSuccess) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n := 3
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n ::= 3
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, InferredUninferralbe) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n := []
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "uninferrable-type")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n ::= []
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "uninferrable-type")));
  }
}

TEST(Declaration, InferredAndUninitialized) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n := --
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "uninferrable-type")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n ::= --
    )");
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "uninferrable-type"),
                             Pair("type-error", "uninitialized-constant")));
  }
}

TEST(Declaration, CustomInitSuccess) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: int64 = 3
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: int64 = 3
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, CustomInitTypeNotAType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: 3 = 4
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: 3  = 4
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }
}

TEST(Declaration, CustomInitNonConstantType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := int64
    n: T = 3
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := int64
    n :: T = 3
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }
}

TEST(Declaration, CustomInitAllowsConversions) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: [0; int64] = []
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Arr(0, type::Int64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: [0; int64] = []
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Arr(0, type::Int64)));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, UninitializedSuccess) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: int64 = --
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::NonConstant(type::Int64));
    EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: int64 = --
    )");
    auto const *qt = mod.data().qual_type(mod.Append<ast::Identifier>("n"));
    ASSERT_NE(qt, nullptr);
    EXPECT_EQ(*qt, type::QualType::Constant(type::Int64));
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "uninitialized-constant")));
  }
}

TEST(Declaration, UninitializedTypeNotAType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n: 3 = --
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    n :: 3  = --
    )");
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "not-a-type"),
                             Pair("type-error", "uninitialized-constant")));
  }
}

TEST(Declaration, UninitializedNonConstantType) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := int64
    n: T = --
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    T := int64
    n :: T = --
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration"),
                    Pair("type-error", "uninitialized-constant")));
  }
}

TEST(Declaration, NonModuleHole) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    -- := 3
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "declaring-hole-as-non-module")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    -- ::= 3
    )");
    EXPECT_THAT(mod.consumer.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "declaring-hole-as-non-module")));
  }
}

TEST(Declaration, NoShadowing) {
  test::TestModule mod;
  mod.AppendCode(R"(
    x := 3
    y := 4
    )");
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Declaration, Shadowing) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    x := 3
    x := 4
    )");
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }

  {
    test::TestModule mod;
    mod.AppendCode(R"(
    x := 3
    x := () => 0
    )");
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
}

TEST(Declaration, FunctionsCanShadow) {
  test::TestModule mod;
  mod.AppendCode(R"(
    f ::= (n: int64) => n
    f ::= (b: bool) => b
    )");
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Declaration, AmbiguouslyCallableFunctionsCannotShadow) {
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    f ::= (n := 0) => n
    f ::= (b := true) => b
    )");
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
  {
    test::TestModule mod;
    mod.AppendCode(R"(
    f ::= (n: [0; int64]) => 0
    f ::= (b: [0; bool]) => 0
    )");
    EXPECT_THAT(
        mod.consumer.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
}

// TODO check shadowing on generics once you have interfaces implemented.
// TODO Special functions (copy, move, etc)

}  // namespace
}  // namespace compiler
