#ifndef ICARUS_TEST_UTIL_H
#define ICARUS_TEST_UTIL_H

#include <string>
#include <memory>

#include "ast/statements.h"
#include "core/scope.h"
#include "frontend/source.h"
#include "misc/context.h"
#include "misc/module.h"

namespace frontend {
std::unique_ptr<ast::Statements> Parse(Src *src, ::Module *mod);
}  // namespace frontend

namespace test {
template <typename T>
std::unique_ptr<T> MakeVerified(std::string s, ::Context *ctx,
                                core::Scope *scope = nullptr) {
  frontend::StringSrc src(std::move(s));
  std::unique_ptr<ast::Statements> stmts = frontend::Parse(&src, ctx->mod_);
  if (!stmts) { return nullptr; }
  if (stmts->content_.size() != 1u) { return nullptr; }
  std::unique_ptr<ast::Node> stmt = std::move(stmts->content_[0]);
  auto *cast_ptr                  = stmt->if_as<T>();
  if (cast_ptr) {
    cast_ptr->assign_scope(scope);
    if (!cast_ptr->VerifyType(ctx)) { return nullptr; }
    stmt.release();
    return std::unique_ptr<T>{cast_ptr};

  } else {
    return nullptr;
  }
}
}  // namespace test

#endif  // ICARUS_TEST_UTIL_H
