#include "misc/context.h"
#include "ast/declaration.h"
#include "ast/dispatch_table.h"
#include "ast/expression.h"
#include "misc/module.h"

type::Type const *Context::type_of(ast::Expression const *expr) const {
  if (auto *decl = expr->if_as<ast::Declaration>()) {
    if (auto *t = current_constants_.type_of(decl)) { return t; }
  }
  if (auto iter = constants_->second.verify_results_.find(expr);
      iter != constants_->second.verify_results_.end()) {
    if (iter->second.type_) { return iter->second.type_; }
  }

  // When searching in embedded modules we intentionally look with no bound
  // constants. Across module boundaries, a declaration can't be present anyway.
  for (Module const *mod : mod_->scope_.embedded_modules_) {
    // TODO use right constants
    if (auto iter = mod->dep_data_.front().second.verify_results_.find(expr);
        iter != mod->dep_data_.front().second.verify_results_.end()) {
      return iter->second.type_;
    }
  }
  return nullptr;
}

visitor::VerifyResult const *Context::prior_verification_attempt(
    ast::ExprPtr expr) {
  auto const &map = constants_->second.verify_results_;
  if (auto iter = map.find(expr); iter != map.end()) { return &iter->second; }
  return nullptr;
}

std::pair<ConstantBinding, Module::DependentData> *Context::insert_constants(
    ConstantBinding const &constant_binding) {
  auto *pair = mod_->insert_constants(constant_binding);
  for (auto const &[decl, binding] : constant_binding.keys_) {
    pair->second.verify_results_.emplace(
        ast::ExprPtr(decl), visitor::VerifyResult::Constant(binding.type_));
  }
  return pair;
}

void Context::set_addr(ast::Declaration const *decl, ir::Reg r) {
  constants_->second.addr_[decl] = r;
}

core::PendingModule *Context::pending_module(
    ast::Import const *import_node) const {
  if (auto iter = constants_->second.imported_module_.find(import_node);
      iter != constants_->second.imported_module_.end()) {
    return &iter->second;
  }
  return nullptr;
}

void Context::set_pending_module(ast::Import const *import_node,
                                 core::PendingModule mod) {
  constants_->second.imported_module_.emplace(import_node, std::move(mod));
}

ir::Reg Context::addr(ast::Declaration const *decl) const {
  return constants_->second.addr_.at(decl);
}

visitor::VerifyResult Context::set_result(ast::ExprPtr expr,
                                              visitor::VerifyResult r) {
  constants_->second.verify_results_.emplace(expr, r);
  return r;
}

void Context::set_dispatch_table(ast::ExprPtr expr,
                                 ast::DispatchTable &&table) {
  constants_->second.dispatch_tables_.emplace(expr, std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

ast::DispatchTable const *Context::dispatch_table(ast::ExprPtr expr) const {
  auto &table = constants_->second.dispatch_tables_;
  if (auto iter = table.find(expr); iter != table.end()) {
    return &iter->second;
  }
  return nullptr;
}
