#include "compiler/dispatch/extract_params.h"

#include "ast/ast.h"
#include "test/catch.h"
#include "test/module.h"
#include "test/util.h"
#include "type/function.h"

namespace compiler {
namespace {
template <typename NodeType>
NodeType const *Make(test::TestModule *mod, std::string code) {
  auto node       = test::ParseAs<NodeType>(std::move(code));
  auto const *ptr = node.get();
  mod->Process(std::move(node));
  return ptr;
}

TEST_CASE("FunctionLiteral") {
  test::TestModule mod;

  SECTION("() -> ()") {
    auto params = ExtractParams(
        &mod.compiler, type::Typed<ast::FunctionLiteral const *>{
                           Make<ast::FunctionLiteral>(&mod, "() -> () {}"),
                           type::Func({}, {})});
    CHECK(params.size() == 0);
  }

  SECTION("(b: bool) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        type::Typed<ast::FunctionLiteral const *>{
            Make<ast::FunctionLiteral>(&mod, "(b: bool) -> () {}"),
            type::Func({}, {})});
    CHECK(params.size() == 1);
    CHECK(params.at(0).name == "b");
    CHECK(params.at(0).value.type() == type::Bool);
  }

  SECTION("(b: bool, n: int32) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        type::Typed<ast::FunctionLiteral const *>{
            Make<ast::FunctionLiteral>(&mod, "(b: bool, n: int32) -> () {}"),
            type::Func({}, {})});
    CHECK(params.size() == 2);
    CHECK(params.at(0).name == "b");
    CHECK(params.at(0).value.type() == type::Bool);
    CHECK(params.at(1).name == "n");
    CHECK(params.at(1).value.type() == type::Int32);
  }
}

TEST_CASE("Constant declaration") {
  test::TestModule mod;

  SECTION("() -> ()") {
    auto params = ExtractParams(
        &mod.compiler, type::Typed<ast::Declaration const *>{
                           Make<ast::Declaration>(&mod, "f ::= () -> () {}"),
                           type::Func({}, {})});
    CHECK(params.size() == 0);
  }

  SECTION("(b: bool) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        type::Typed<ast::Declaration const *>{
            Make<ast::Declaration>(&mod, "f ::= (b: bool) -> () {}"),
            type::Func({type::Bool}, {})});
    CHECK(params.size() == 1);
    CHECK(params.at(0).name == "b");
    CHECK(params.at(0).value.type() == type::Bool);
  }

  SECTION("(b: bool, n: int32) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        type::Typed<ast::Declaration const *>{
            Make<ast::Declaration>(&mod, "f ::= (b: bool, n: int32) -> () {}"),
            type::Func({type::Bool, type::Int32}, {})});
    CHECK(params.size() == 2);
    CHECK(params.at(0).name == "b");
    CHECK(params.at(0).value.type() == type::Bool);
    CHECK(params.at(1).name == "n");
    CHECK(params.at(1).value.type() == type::Int32);
  }
}

TEST_CASE("Non-constant declaration") {
  test::TestModule mod;

  SECTION("() -> ()") {
    auto params = ExtractParams(
        &mod.compiler, type::Typed<ast::Declaration const *>{
                           Make<ast::Declaration>(&mod, "f := () -> () {}"),
                           type::Func({}, {})});
    CHECK(params.size() == 0);
  }

  SECTION("(b: bool) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        type::Typed<ast::Declaration const *>{
            Make<ast::Declaration>(&mod, "f := (b: bool) -> () {}"),
            type::Func({type::Bool}, {})});
    CHECK(params.size() == 1);
    CHECK(params.at(0).name == "");
    CHECK(params.at(0).value.type() == type::Bool);
  }

  SECTION("(b: bool, n: int32) -> ()") {
    auto params = ExtractParams(
        &mod.compiler,
        type::Typed<ast::Declaration const *>{
            Make<ast::Declaration>(&mod, "f := (b: bool, n: int32) -> () {}"),
            type::Func({type::Bool, type::Int32}, {})});
    CHECK(params.size() == 2);
    CHECK(params.at(0).name == "");
    CHECK(params.at(0).value.type() == type::Bool);
    CHECK(params.at(1).name == "");
    CHECK(params.at(1).value.type() == type::Int32);
  }
}
}  // namespace
}  // namespace compiler
