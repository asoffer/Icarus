#include "compiler/dispatch/match.h"

#include "ast/ast.h"
#include "compiler/dispatch/extract_params.h"
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

core::FnParams<type::Typed<ast::Declaration const *>> MakeParams(
    test::TestModule *mod, std::string code, type::Function const *fn_type) {
  return ExtractParams(
      &mod->compiler,
      type::Typed<ast::FunctionLiteral const *>{
          Make<ast::FunctionLiteral>(mod, std::move(code)), fn_type});
}

TEST_CASE("() -> () {}") {
  test::TestModule mod;
  auto params = MakeParams(&mod, "() -> () {}", type::Func({}, {}));

  SECTION("() -> ()") {
    auto args           = core::FnArgs<compiler::VerifyResult>({}, {});
    auto matched_params = MatchArgsToParams(params, args);
    // TODO actually test things.
  }

}

TEST_CASE("(n: int32) -> () {}") {
  test::TestModule mod;
  auto params = MakeParams(&mod, "(n: int32) -> () {}", type::Func({}, {}));

  SECTION("Call without args") {
    auto args           = core::FnArgs<compiler::VerifyResult>({}, {});
    auto matched_params = MatchArgsToParams(params, args);
    // TODO actually test things.
  }
}

}  // namespace
}  // namespace compiler
