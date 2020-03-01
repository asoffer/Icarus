#include "compiler/dispatch/fn_call_table.h"

#include "compiler/compiler.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "test/util.h"

namespace compiler {

TEST(FnCallDispatchTable, TrivialFunction) {
  test::TestModule mod;
  auto *fn = mod.Append<ast::Declaration>("f ::= () -> () {}");
  auto q   = type::QualType::Constant(type::Int64);

  EXPECT_TRUE(FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({fn}),
                                          core::FnArgs<type::QualType>{
                                              /* pos = */ {}, /* named = */ {}})
                  .has_value());
  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::QualType>{/* pos = */ {q}, /* named = */ {}})
          .has_value());
  EXPECT_FALSE(FnCallDispatchTable::Verify(
                   &mod.compiler, ast::OverloadSet({fn}),
                   core::FnArgs<type::QualType>{/* pos = */ {},
                                                /* named = */ {{"a", q}}})
                   .has_value());
}

TEST(FnCallDispatchTable, ConstantBoolToVoid) {
  test::TestModule mod;
  auto *fn = mod.Append<ast::Declaration>("f ::= (b: bool) -> () {}");
  auto q   = type::QualType::Constant(type::Bool);

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::QualType>{/* pos = */ {q}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {},
                                               /* named = */ {{"b", q}}})
                  .has_value());
  EXPECT_FALSE(FnCallDispatchTable::Verify(
                   &mod.compiler, ast::OverloadSet({fn}),
                   core::FnArgs<type::QualType>{/* pos = */ {},
                                                /* named = */ {{"c", q}}})
                   .has_value());
}

TEST(FnCallDispatchTable, NonConstantBoolToVoid) {
  test::TestModule mod;
  auto *fn = mod.Append<ast::Declaration>("f := (b: bool) -> () {}");
  auto q   = type::QualType::Constant(type::Bool);

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::QualType>{/* pos = */ {q}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {},
                                               /* named = */ {{"b", q}}})
                  .has_value());
  EXPECT_FALSE(FnCallDispatchTable::Verify(
                   &mod.compiler, ast::OverloadSet({fn}),
                   core::FnArgs<type::QualType>{/* pos = */ {},
                                                /* named = */ {{"c", q}}})
                   .has_value());
}

TEST(FnCallDispatchTable, SimpleOverloadSet) {
  test::TestModule mod;
  auto *f1 = mod.Append<ast::Declaration>("f ::= (x: bool | type) -> () {}");
  auto *f2 = mod.Append<ast::Declaration>("f ::= (x: int64 | *int64) -> () {}");

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::QualType>{
                      /* pos = */ {type::QualType::NonConstant(type::Bool)},
                      /* named = */ {}})
                  .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::QualType>{
                      /* pos = */ {type::QualType::NonConstant(type::Int64)},
                      /* named = */ {}})
                  .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::QualType>{
                      /* pos = */ {type::QualType::NonConstant(
                          type::Var({type::Bool, type::Int64}))},
                      /* named = */ {}})
                  .has_value());

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{
              /* pos = */ {},
              /* named = */ {{"b", type::QualType::NonConstant(type::Bool)}}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{
              /* pos = */ {},
              /* named = */ {{"x", type::QualType::NonConstant(type::Bool)}}})
          .has_value());
}

TEST(FnCallDispatchTable, OverloadSetWithDefaults) {
  test::TestModule mod;
  auto *f1 = mod.Append<ast::Declaration>("f ::= (x: bool) -> () {}");
  auto *f2 = mod.Append<ast::Declaration>("f ::= (x: int64, y := 0) -> () {}");

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::QualType>{
                      /* pos = */ {type::QualType::NonConstant(type::Bool)},
                      /* named = */ {}})
                  .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::QualType>{
                      /* pos = */ {type::QualType::NonConstant(type::Int64)},
                      /* named = */ {}})
                  .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::QualType>{
                      /* pos = */ {type::QualType::NonConstant(
                          type::Var({type::Bool, type::Int64}))},
                      /* named = */ {}})
                  .has_value());

  EXPECT_TRUE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{
              /* pos = */ {type::QualType::NonConstant(type::Int64)},
              /* named = */ {{"y", type::QualType::NonConstant(type::Int64)}}})
          .has_value());
  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{
              /* pos = */ {type::QualType::NonConstant(
                  type::Var({type::Bool, type::Int64}))},
              /* named = */ {{"y", type::QualType::NonConstant(type::Int64)}}})
          .has_value());
}

}  // namespace compiler
