#include "compiler/dispatch/fn_call_table.h"

#include "compiler/compiler.h"
#include "test/catch.h"
#include "test/module.h"
#include "test/util.h"

namespace compiler {
template <typename NodeType>
NodeType const *Make(test::TestModule *mod, std::string code) {
  auto node       = test::ParseAs<NodeType>(std::move(code));
  auto const *ptr = node.get();
  mod->AppendNode(std::move(node));
  return ptr;
}

TEST_CASE("() -> ()") {
  test::TestModule mod;
  auto *fn = Make<ast::Declaration>(&mod, "f ::= () -> () {}");
  auto q   = type::QualType::Constant(type::Int64);

  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({fn}),
            core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
            .has_value());
  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {q}, /* named = */ {}})
                  .has_value());
  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {},
                                             /* named = */ {{"a", q}}})
                  .has_value());
}

TEST_CASE("constant (b: bool) -> ()") {
  test::TestModule mod;
  auto *fn = Make<ast::Declaration>(&mod, "f ::= (b: bool) -> () {}");
  auto q   = type::QualType::Constant(type::Bool);

  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
                  .has_value());
  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({fn}),
            core::FnArgs<type::QualType>{/* pos = */ {q}, /* named = */ {}})
            .has_value());
  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({fn}),
            core::FnArgs<type::QualType>{/* pos = */ {},
                                       /* named = */ {{"b", q}}})
            .has_value());
  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {},
                                             /* named = */ {{"c", q}}})
                  .has_value());
}

TEST_CASE("non-constant (b: bool) -> ()") {
  test::TestModule mod;
  auto *fn = Make<ast::Declaration>(&mod, "f := (b: bool) -> () {}");
  auto q   = type::QualType::Constant(type::Bool);

  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
                  .has_value());
  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({fn}),
            core::FnArgs<type::QualType>{/* pos = */ {q}, /* named = */ {}})
            .has_value());
  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {},
                                             /* named = */ {{"b", q}}})
                  .has_value());
  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({fn}),
                  core::FnArgs<type::QualType>{/* pos = */ {},
                                             /* named = */ {{"c", q}}})
                  .has_value());
}

TEST_CASE("simple overload-set") {
  test::TestModule mod;
  auto *f1 = Make<ast::Declaration>(&mod, "f ::= (x: bool | type) -> () {}");
  auto *f2 = Make<ast::Declaration>(&mod, "f ::= (x: int64 | *int64) -> () {}");

  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
                  .has_value());
  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({f1, f2}),
            core::FnArgs<type::QualType>{
                /* pos = */ {type::QualType::NonConstant(type::Bool)},
                /* named = */ {}})
            .has_value());
  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({f1, f2}),
            core::FnArgs<type::QualType>{
                /* pos = */ {type::QualType::NonConstant(type::Int64)},
                /* named = */ {}})
            .has_value());
  CHECK(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{/* pos = */ {type::QualType::NonConstant(
                                         type::Var({type::Bool, type::Int64}))},
                                     /* named = */ {}})
          .has_value());

  CHECK_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{
              /* pos = */ {},
              /* named = */ {{"b", type::QualType::NonConstant(type::Bool)}}})
          .has_value());
  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({f1, f2}),
            core::FnArgs<type::QualType>{
                /* pos = */ {},
                /* named = */ {{"x", type::QualType::NonConstant(type::Bool)}}})
            .has_value());
}

TEST_CASE("overload-set with defaults") {
  test::TestModule mod;
  auto *f1 = Make<ast::Declaration>(&mod, "f ::= (x: bool) -> () {}");
  auto *f2 = Make<ast::Declaration>(&mod, "f ::= (x: int64, y := 0) -> () {}");

  CHECK_FALSE(FnCallDispatchTable::Verify(
                  &mod.compiler, ast::OverloadSet({f1, f2}),
                  core::FnArgs<type::QualType>{/* pos = */ {}, /* named = */ {}})
                  .has_value());
  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({f1, f2}),
            core::FnArgs<type::QualType>{
                /* pos = */ {type::QualType::NonConstant(type::Bool)},
                /* named = */ {}})
            .has_value());
  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({f1, f2}),
            core::FnArgs<type::QualType>{
                /* pos = */ {type::QualType::NonConstant(type::Int64)},
                /* named = */ {}})
            .has_value());
  CHECK(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{/* pos = */ {type::QualType::NonConstant(
                                         type::Var({type::Bool, type::Int64}))},
                                     /* named = */ {}})
          .has_value());

  CHECK(FnCallDispatchTable::Verify(
            &mod.compiler, ast::OverloadSet({f1, f2}),
            core::FnArgs<type::QualType>{
                /* pos = */ {type::QualType::NonConstant(type::Int64)},
                /* named = */ {{"y", type::QualType::NonConstant(type::Int64)}}})
            .has_value());
  CHECK_FALSE(
      FnCallDispatchTable::Verify(
          &mod.compiler, ast::OverloadSet({f1, f2}),
          core::FnArgs<type::QualType>{
              /* pos = */ {type::QualType::NonConstant(
                  type::Var({type::Bool, type::Int64}))},
              /* named = */ {{"y", type::QualType::NonConstant(type::Int64)}}})
          .has_value());
}

}  // namespace compiler
