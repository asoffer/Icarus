#include "misc/context.h"
#include "ast/dispatch_table.h"
#include "ast/expression.h"
#include "misc/module.h"

type::Type const *Context::type_of(ast::Expression const *expr) const {
  if (auto *result = mod_->type_of(bound_constants_, expr)) { return result; }
  if (parent_) { return parent_->type_of(expr); }

  // When searching in embedded modules we intentionally look with no bound
  // constants. Across module boundaries, a declaration can't be present anyway.
  for (Module const *mod : mod_->scope_.embedded_modules_) {
    auto bc_iter = mod->data_.find(ast::BoundConstants{});
    if (bc_iter == mod->data_.end()) { continue; }

    if (auto iter = bc_iter->second.verify_results_.find(expr);
        iter != bc_iter->second.verify_results_.end()) {
      return iter->second.type_;
    }
  }
  return nullptr;
}

ast::VerifyResult const *Context::prior_verification_attempt(
    ast::Expression const *expr) {
  auto const &map = mod_->data_[bound_constants_].verify_results_;
  if (auto iter = map.find(expr); iter != map.end()) { return &iter->second; }
  return nullptr;
}

void Context::set_addr(ast::Declaration *decl, ir::Register r) {
  mod_->data_[bound_constants_].addr_[decl] = r;
}

ir::Register Context::addr(ast::Declaration *decl) const {
  return mod_->addr(bound_constants_, decl);
}

ast::VerifyResult Context::set_result(ast::Expression const *expr,
                                      ast::VerifyResult r) {
  mod_->data_[bound_constants_].verify_results_.emplace(expr, r);
  return r;
}

void Context::set_dispatch_table(ast::Expression const *expr,
                                 ast::DispatchTable &&table) {
  mod_->data_[bound_constants_].dispatch_tables_.emplace(expr,
                                                         std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

ast::DispatchTable const *Context::dispatch_table(ast::Expression const *expr) const {
  auto &table = mod_->data_[bound_constants_].dispatch_tables_;
  if (auto iter = table.find(expr); iter != table.end()) {
    return &iter->second;
  }
  if (parent_) { return parent_->dispatch_table(expr); }
  return nullptr;
}
