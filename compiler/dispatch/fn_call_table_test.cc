#include "compiler/dispatch/fn_call_table.h"

#include "compiler/compiler.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "test/util.h"

namespace compiler {

TEST(FnCallDispatchTable, TrivialFunction) {
  test::TestModule mod;
  auto *fn = mod.Append<ast::Declaration>("f ::= () -> () {}");
  auto r   = type::Typed<ir::Results>(ir::Results{3}, type::Int64);

  EXPECT_TRUE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({fn}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_FALSE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({fn}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {r}, /* named = */ {}})
          .has_value());
  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::Typed<ir::Results>>{/* pos = */ {},
                                                 /* named = */ {{"a", r}}})
          .has_value());
}

TEST(FnCallDispatchTable, ConstantBoolToVoid) {
  test::TestModule mod;
  auto *fn = mod.Append<ast::Declaration>("f ::= (b: bool) -> () {}");
  auto r   = type::Typed<ir::Results>(ir::Results{true}, type::Bool);

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({fn}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({fn}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {r}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::Typed<ir::Results>>{/* pos = */ {},
                                                 /* named = */ {{"b", r}}})
          .has_value());
  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::Typed<ir::Results>>{/* pos = */ {},
                                                 /* named = */ {{"c", r}}})
          .has_value());
}

TEST(FnCallDispatchTable, NonConstantBoolToVoid) {
  test::TestModule mod;
  auto *fn = mod.Append<ast::Declaration>("f := (b: bool) -> () {}");
  auto r   = type::Typed<ir::Results>(ir::Results{}, type::Bool);

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({fn}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({fn}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {r}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::Typed<ir::Results>>{/* pos = */ {},
                                                 /* named = */ {{"b", r}}})
          .has_value());
  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({fn}),
          core::FnArgs<type::Typed<ir::Results>>{/* pos = */ {},
                                                 /* named = */ {{"c", r}}})
          .has_value());
}

TEST(FnCallDispatchTable, SimpleOverloadSet) {
  test::TestModule mod;
  auto *f1 = mod.Append<ast::Declaration>("f ::= (x: bool | type) -> () {}");
  auto *f2 = mod.Append<ast::Declaration>("f ::= (x: int64 | *int64) -> () {}");

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({f1, f2}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::Typed<ir::Results>>{
              /* pos = */ {type::Typed<ir::Results>(ir::Results{}, type::Bool)},
              /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({f1, f2}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {type::Typed<ir::Results>(
                                          ir::Results{}, type::Int64)},
                                      /* named = */ {}})
          .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::Typed<ir::Results>>{
                      /* pos = */ {type::Typed<ir::Results>(
                          ir::Results{}, type::Var({type::Bool, type::Int64}))},
                      /* named = */ {}})
                  .has_value());

  EXPECT_FALSE(FnCallDispatchTable::Verify(
                   &mod.compiler, ast::OverloadSet({f1, f2}),
                   core::FnArgs<type::Typed<ir::Results>>{
                       /* pos = */ {},
                       /* named = */ {{"b", type::Typed<ir::Results>(
                                                ir::Results{}, type::Bool)}}})
                   .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::Typed<ir::Results>>{
                      /* pos = */ {},
                      /* named = */ {{"x", type::Typed<ir::Results>(
                                               ir::Results{}, type::Bool)}}})
                  .has_value());
}

TEST(FnCallDispatchTable, OverloadSetWithDefaults) {
  test::TestModule mod;
  auto *f1 = mod.Append<ast::Declaration>("f ::= (x: bool) -> () {}");
  auto *f2 = mod.Append<ast::Declaration>("f ::= (x: int64, y := 0) -> () {}");

  EXPECT_FALSE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({f1, f2}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {}, /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::Typed<ir::Results>>{
              /* pos = */ {type::Typed<ir::Results>(ir::Results{}, type::Bool)},
              /* named = */ {}})
          .has_value());
  EXPECT_TRUE(
      FnCallDispatchTable::Verify(&mod.compiler, ast::OverloadSet({f1, f2}),
                                  core::FnArgs<type::Typed<ir::Results>>{
                                      /* pos = */ {type::Typed<ir::Results>(
                                          ir::Results{}, type::Int64)},
                                      /* named = */ {}})
          .has_value());
  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::Typed<ir::Results>>{
                      /* pos = */ {type::Typed<ir::Results>(
                          ir::Results{}, type::Var({type::Bool, type::Int64}))},
                      /* named = */ {}})
                  .has_value());

  EXPECT_TRUE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::Typed<ir::Results>>{
                      /* pos = */ {
                          type::Typed<ir::Results>(ir::Results{}, type::Int64)},
                      /* named = */ {{"y", type::Typed<ir::Results>(
                                               ir::Results{}, type::Int64)}}})
                  .has_value());
  EXPECT_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::Typed<ir::Results>>{
              /* pos = */ {type::Typed<ir::Results>(
                  ir::Results{}, type::Var({type::Bool, type::Int64}))},
              /* named = */ {{"y", type::Typed<ir::Results>(ir::Results{},
                                                            type::Int64)}}})
          .has_value());
}

}  // namespace compiler
