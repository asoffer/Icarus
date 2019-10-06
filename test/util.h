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

namespace test {

template <typename T>
struct MakeNodeResult {
  template <bool Verify>
  MakeNodeResult(std::string s, core::Scope *scope,
                 std::integral_constant<bool, Verify>)
      : source(std::move(s)), module(&source), compiler(&module) {
    if (scope == nullptr) { scope = &module.scope_; }
    auto stmts = frontend::Parse(&source);
    if (stmts.size() != 1u) { return; }
    auto *cast_ptr = stmts[0]->template if_as<T>();
    if (cast_ptr) {
      {
        visitor::AssignScope a;
        cast_ptr->assign_scope(&a, scope);
      }

      if constexpr (Verify) {
        if (!cast_ptr->VerifyType(&compiler)) { return; }
      }
      stmts[0].release();
      node = std::unique_ptr<T>{cast_ptr};
    }
  }

  frontend::StringSource source;
  Module module;
  compiler::Compiler compiler;
  std::unique_ptr<T> node;
};

namespace internal {
template <typename T, bool Verify>
MakeNodeResult<T> MakeNode(std::string s, core::Scope *scope) {
  MakeNodeResult<T> result(std::move(s));

}

}  // namespace internal

template <typename T>
MakeNodeResult<T> MakeUnverified(std::string s, core::Scope *scope = nullptr) {
  return MakeNodeResult<T>(std::move(s), scope, std::false_type{});
}

template <typename T>
MakeNodeResult<T> MakeVerified(std::string s, core::Scope *scope = nullptr) {
  return MakeNodeResult<T>(std::move(s), scope, std::true_type{});
}

}  // namespace test

#endif  // ICARUS_TEST_UTIL_H
