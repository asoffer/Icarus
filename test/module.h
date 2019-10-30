#ifndef ICARUS_TEST_MODULE_H
#define ICARUS_TEST_MODULE_H

#include <string>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/ast.h"
#include "ast/expression.h"
#include "ast/overload_set.h"
#include "base/ptr_span.h"
#include "frontend/source/range.h"
#include "module/module.h"
#include "test/util.h"

namespace test {

struct TestModule : module::ExtendedModule<TestModule> {
  TestModule()
      : module::ExtendedModule<TestModule>(
            [this](base::PtrSpan<ast::Node const> nodes) {
              for (ast::Node const* node : nodes) {
                node->VerifyType(&compiler);
              }
            }),
        compiler(this) {}
  compiler::Compiler compiler;
};

std::pair<ast::OverloadSet, ast::Call*> MakeCall(
    std::string fn, std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args,
    TestModule* module) {
  auto call_expr = std::make_unique<ast::Call>(
      frontend::SourceRange{}, ParseAs<ast::Expression>(std::move(fn)),
      MakeFnArgs(std::move(pos_args), std::move(named_args)));
  ast::OverloadSet os;
  os.insert(call_expr->callee());
  auto* expr = call_expr.get();
  module->Process(std::move(call_expr));
  return std::pair(std::move(os), expr);
}

}  // namespace test

#endif  // ICARUS_TEST_MODULE_H