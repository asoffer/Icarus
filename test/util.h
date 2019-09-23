#ifndef ICARUS_TEST_UTIL_H
#define ICARUS_TEST_UTIL_H

#include <memory>
#include <string>

#include "core/scope.h"
#include "frontend/source/string.h"
#include "misc/module.h"
#include "visitor/traditional_compilation.h"

namespace frontend {
std::vector<std::unique_ptr<ast::Node>> Parse(Source *src, ::Module *mod);
}  // namespace frontend

namespace test {
namespace internal {

template <typename T, bool Verify>
std::unique_ptr<T> MakeNode(std::string s,
                            visitor::TraditionalCompilation *visitor,
                            core::Scope *scope) {
  if (scope == nullptr) { scope = &visitor->module()->scope_; }
  frontend::StringSource src(std::move(s));
  auto stmts = frontend::Parse(&src, visitor->module());
  if (stmts.size() != 1u) { return nullptr; }
  auto *cast_ptr = stmts[0]->if_as<T>();
  if (cast_ptr) {
    {
      visitor::AssignScope a;
      cast_ptr->assign_scope(&a, scope);
    }

    if constexpr (Verify) {
      if (!cast_ptr->VerifyType(visitor)) { return nullptr; }
    }
    stmts[0].release();
    return std::unique_ptr<T>{cast_ptr};

  } else {
    return nullptr;
  }
}

}  // namespace internal

template <typename T>
std::unique_ptr<T> MakeUnverified(visitor::TraditionalCompilation *visitor,
                                  std::string s, core::Scope *scope = nullptr) {
  return internal::MakeNode<T, false>(std::move(s), visitor, scope);
}

template <typename T>
std::unique_ptr<T> MakeVerified(visitor::TraditionalCompilation *visitor,
                                std::string s, core::Scope *scope = nullptr) {
  return internal::MakeNode<T, true>(std::move(s), visitor, scope);
}

}  // namespace test

#endif  // ICARUS_TEST_UTIL_H
