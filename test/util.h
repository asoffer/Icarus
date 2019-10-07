#ifndef ICARUS_TEST_UTIL_H
#define ICARUS_TEST_UTIL_H

#include <memory>
#include <string>
#include <type_traits>

#include "compiler/compiler.h"
#include "core/scope.h"
#include "frontend/parse.h"
#include "frontend/source/string.h"
#include "module/module.h"
#include "type/function.h"

namespace test {

template <typename T>
std::unique_ptr<T> ParseAs(std::string s) {
  frontend::StringSource source(std::move(s));
  auto stmts     = frontend::Parse(&source);
  auto* cast_ptr = stmts[0]->template if_as<T>();
  if (!cast_ptr) { return nullptr; }
  stmts[0].release();
  return std::unique_ptr<T>(cast_ptr);
}

core::OrderedFnArgs<ast::Expression> MakeFnArgs(
    std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args) {
  std::vector<std::pair<std::string, std::unique_ptr<ast::Expression>>> vec;
  for (auto pos_arg : pos_args) {
    vec.emplace_back("", ParseAs<ast::Expression>(std::move(pos_arg)));
  }
  for (auto& [name, arg] : named_args) {
    vec.emplace_back(std::move(name), ParseAs<ast::Expression>(std::move(arg)));
  }
  return core::OrderedFnArgs<ast::Expression>(std::move(vec));
}

std::pair<ast::OverloadSet, ast::Call*> MakeCall(
    std::string fn, std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args,
    compiler::Compiler* compiler) {
  auto call_expr = std::make_unique<ast::Call>(
      frontend::SourceRange{}, ParseAs<ast::Expression>(std::move(fn)),
      MakeFnArgs(std::move(pos_args), std::move(named_args)));
  ast::OverloadSet os;
  os.emplace(call_expr->callee(),
             compiler::VerifyResult::Constant(type::Func({}, {})));
  auto* expr = call_expr.get();
  compiler->module()->Append(std::move(call_expr));
  compiler->module()->process(
      [&compiler](base::PtrSpan<ast::Node const> nodes) {
        for (ast::Node const* node : nodes) { node->VerifyType(compiler); }
      });
  return std::pair(std::move(os), expr);
}

}  // namespace test

#endif  // ICARUS_TEST_UTIL_H
