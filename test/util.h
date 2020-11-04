#ifndef ICARUS_TEST_UTIL_H
#define ICARUS_TEST_UTIL_H

#include <memory>
#include <string>
#include <type_traits>

#include "ast/scope/scope.h"
#include "base/macros.h"
#include "compiler/compiler.h"
#include "diagnostic/consumer/aborting.h"
#include "frontend/parse.h"
#include "frontend/source/string.h"
#include "module/module.h"
#include "type/function.h"

namespace test {

template <typename T>
std::unique_ptr<T> ParseAs(
    std::string s, frontend::LineNum initial_line_num = frontend::LineNum(1)) {
  frontend::StringSource source(std::move(s));
  diagnostic::AbortingConsumer diag(&source);
  auto stmts     = frontend::Parse(source, diag, initial_line_num);
  auto* cast_ptr = stmts[0]->template if_as<T>();
  if (not cast_ptr) { return nullptr; }
  stmts[0].release();
  return std::unique_ptr<T>(cast_ptr);
}

core::OrderedArguments<ast::Expression> MakeArguments(
    std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args) {
  std::vector<std::pair<std::string, std::unique_ptr<ast::Expression>>> vec;
  frontend::LineNum line_num(1);
  for (auto pos_arg : pos_args) {
    vec.emplace_back("",
                     ParseAs<ast::Expression>(std::move(pos_arg), line_num));
    ++line_num;
  }
  for (auto& [name, arg] : named_args) {
    vec.emplace_back(std::move(name),
                     ParseAs<ast::Expression>(std::move(arg), line_num));
    ++line_num;
  }
  return core::OrderedArguments<ast::Expression>(std::move(vec));
}

}  // namespace test

#endif  // ICARUS_TEST_UTIL_H
