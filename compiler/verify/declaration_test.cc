#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Return;
using ::testing::UnorderedElementsAre;

TEST(Declaration, DefaultInitSuccess) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n: i64
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::I64, type::Qualifiers::Storage())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n :: i64
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::I64, type::Qualifiers::Constant())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, DefaultInitTypeNotAType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n: 3 
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n :: 3 
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }
}

TEST(Declaration, DefaultInitNonConstantType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    T := i64
    n: T 
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    T := i64
    n :: T
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }
}

TEST(Declaration, DefaultInitNonDefaultInitializableType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    s: [/]char
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "no-default-value")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    s :: [/]char
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "no-default-value")));
  }
}

TEST(Declaration, InferredSuccess) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n := 3
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::I64, type::Qualifiers::Storage())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n ::= 3
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::Integer, type::Qualifiers::Constant())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, InferredUninferrable) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n := []
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "uninferrable-type")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n ::= []
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "uninferrable-type")));
  }
}

TEST(Declaration, InferredAndUninitialized) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n := --
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "uninferrable-type")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n ::= --
    )");
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "uninferrable-type"),
                             Pair("type-error", "uninitialized-constant")));
  }
}

TEST(Declaration, CustomInitSuccess) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n: i64 = 3
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::I64, type::Qualifiers::Storage())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n :: i64 = 3
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::I64, type::Qualifiers::Constant())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, CustomInitTypeNotAType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n: 3 = 4
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n :: 3  = 4
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }
}

TEST(Declaration, CustomInitNonConstantType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    T := i64
    n: T = 3
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    T := i64
    n :: T = 3
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }
}

TEST(Declaration, CustomInitAllowsConversions) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n: [0; i64] = []
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::Arr(0, type::I64), type::Qualifiers::Buffer())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  // TODO: `SetArrayInits` will store byte code to the `ir::Module` in `infra`
  // but does so with a once_flag meaning the second incantation here will not
  // recreate the byte code even though we have a new `ir::Module`. This is a
  // bug that only manifests in tests and we are currently working around it by
  // using a different type so we hit a unique `once_flag`. But this behavior is
  // obviously bad and should be fixed.
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n :: [0; i32] = []
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts,
                UnorderedElementsAre(type::QualType(
                    type::Arr(0, type::I32), type::Qualifiers::Constant())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, UninitializedSuccess) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n: i64 = --
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::I64, type::Qualifiers::Storage())));
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n :: i64 = --
    n
    )");
    auto qts  = mod.context().qual_types(mod.get<ast::Identifier>());
    EXPECT_THAT(qts, UnorderedElementsAre(type::QualType(
                         type::I64, type::Qualifiers::Constant())));
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "uninitialized-constant")));
  }
}

TEST(Declaration, UninitializedTypeNotAType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n: 3 = --
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(Pair("type-error", "not-a-type")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    n :: 3  = --
    )");
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "not-a-type"),
                             Pair("type-error", "uninitialized-constant")));
  }
}

TEST(Declaration, UninitializedNonConstantType) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    T := i64
    n: T = --
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    T := i64
    n :: T = --
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "non-constant-type-in-declaration"),
                    Pair("type-error", "uninitialized-constant")));
  }
}

TEST(Declaration, NonModuleHole) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    -- := 3
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "declaring-hole-as-non-module")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    -- ::= 3
    )");
    EXPECT_THAT(infra.diagnostics(),
                UnorderedElementsAre(
                    Pair("type-error", "declaring-hole-as-non-module")));
  }
}

TEST(Declaration, NoShadowing) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
    x := 3
    y := 4
    )");
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Declaration, Shadowing) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    x := 3
    x := 4
    )");
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }

  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    x := 3
    x := () => 0
    )");
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
}

TEST(Declaration, ShadowingOkayIfMutuallyUnreachable) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
    if (true) {
      x := 3
    } else {
      x := 4
    }
    )");
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Declaration, ShadowingCaughtAcrossScopes) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    x := 3
    if (true) {
      x := 4
    }
    )");
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    if (true) {
      x := 3
    }
    x := 4
    )");
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    if (true) {
      x := 3
    }
    x ::= 4
    )");
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
}

TEST(Declaration, FunctionsCanShadow) {
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(
    f ::= (n: i64) => n
    f ::= (b: bool) => b
    )");
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Declaration, AmbiguouslyCallableFunctionsCannotShadow) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    f ::= (n := 0) => n
    f ::= (b := true) => b
    )");
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    f ::= (n: [0; i64]) => 0
    f ::= (b: [0; bool]) => 0
    )");
    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
}

TEST(Declaration, StructFieldsCanShadow) {
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    x: bool

    S ::= struct {
      x: i64
    }

    s := S.{ x = 3 }
    )");
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &mod = infra.add_module(R"(
    S ::= struct {
      x: i64
    }

    s := S.{ x = 3 }

    x: bool
    )");
    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, ShadowingAcrossEmbeddedModules) {
  {
    test::CompilerInfrastructure infra;
    auto &imported = infra.add_module("imported", R"(
    #{export} x :: i64 = 1
    )");

    auto &mod = infra.add_module(R"(
    -- ::= import "imported"
    y :: i64 = 2
    )");

    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &imported = infra.add_module("imported", R"(
    #{export} x :: i64 = 1
    )");

    auto &mod = infra.add_module(R"(
    -- ::= import "imported"
    x :: i64 = 2
    )");

    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
  {
    test::CompilerInfrastructure infra;
    auto &imported = infra.add_module("imported", R"(
    #{export} f ::= () => true
    )");

    auto &mod = infra.add_module(R"(
    -- ::= import "imported"
    f ::= (n: i64 = 0) => n
    )");

    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
  {
    test::CompilerInfrastructure infra;
    auto &imported = infra.add_module("imported", R"(
    #{export} f ::= () => true
    )");

    auto &mod = infra.add_module(R"(
    -- ::= import "imported"
    f ::= (n: i64) => n
    )");

    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
}

TEST(Declaration, MultiEmbeddedImportShadowing) {
  {
    test::CompilerInfrastructure infra;
    auto &imported1 = infra.add_module("imported1", R"(
    #{export} x :: i64 = 1
    )");
    auto &imported2 = infra.add_module("imported2", R"(
    #{export} y :: i64 = 2
    )");

    auto &mod = infra.add_module(R"(
    -- ::= import "imported1"
    -- ::= import "imported2"
    )");

    EXPECT_THAT(infra.diagnostics(), IsEmpty());
  }
  {
    test::CompilerInfrastructure infra;
    auto &imported1 = infra.add_module("imported1", R"(
    #{export} x :: i64 = 1
    )");
    auto &imported2 = infra.add_module("imported2", R"(
    #{export} x :: i64 = 2
    )");

    auto &mod = infra.add_module(R"(
    -- ::= import "imported1"
    -- ::= import "imported2"
    )");

    EXPECT_THAT(
        infra.diagnostics(),
        UnorderedElementsAre(Pair("type-error", "shadowing-declaration")));
  }
}

TEST(BindingDeclaration, Success) {
  test::CompilerInfrastructure infra;
  auto &mod     = infra.add_module(R"(
  true ~ `x
  x
  )");
  auto const *e = mod.get<ast::Expression>();
  ASSERT_THAT(mod.context().qual_types(e),
              ElementsAre(type::QualType::Constant(type::Bool)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

// TODO: check shadowing on generics once you have interfaces implemented.

}  // namespace
}  // namespace compiler
