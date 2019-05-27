#ifndef ICARUS_TEST_UTIL_H
#define ICARUS_TEST_UTIL_H

#include <string>
#include <memory>

#include "core/scope.h"
#include "frontend/source.h"
#include "misc/context.h"
#include "misc/module.h"

namespace frontend {
std::vector<std::unique_ptr<ast::Node>> Parse(Src *src, ::Module *mod);
}  // namespace frontend

namespace test {
namespace internal {

template <typename T, bool Verify>
std::unique_ptr<T> MakeNode(std::string s, ::Context *ctx, core::Scope *scope) {
  if (scope == nullptr) { scope = &ctx->mod_->scope_; }
  frontend::StringSrc src(std::move(s));
  auto stmts = frontend::Parse(&src, ctx->mod_);
  if (stmts.size() != 1u) { return nullptr; }
  auto *cast_ptr = stmts[0]->if_as<T>();
  if (cast_ptr) {
    {
      visitor::AssignScope visitor;
      cast_ptr->assign_scope(&visitor, scope);
    }
    visitor::VerifyType visitor;
    if (Verify && !cast_ptr->VerifyType(&visitor, ctx)) { return nullptr; }
    stmts[0].release();
    return std::unique_ptr<T>{cast_ptr};

  } else {
    return nullptr;
  }
}

}  // namespace internal

template <typename T>
std::unique_ptr<T> MakeUnverified(std::string s, ::Context *ctx,
                                  core::Scope *scope = nullptr) {
  return internal::MakeNode<T, false>(std::move(s), ctx, scope);
}

template <typename T>
std::unique_ptr<T> MakeVerified(std::string s, ::Context *ctx,
                                  core::Scope *scope = nullptr) {
  return internal::MakeNode<T, true>(std::move(s), ctx, scope);
}

}  // namespace test

#endif  // ICARUS_TEST_UTIL_H
