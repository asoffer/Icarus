#include "visitor/emit_ir.h"

#include "ast/declaration.h"
#include "ast/expression.h"
#include "misc/module.h"

namespace visitor {

EmitIr::EmitIr(Module *mod) : mod_(ASSERT_NOT_NULL(mod)) {
  constants_ = &mod_->dep_data_.front();
}

type::Type const *EmitIr::type_of(ast::Expression const *expr) const {
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

ir::Reg EmitIr::addr_of(ast::Declaration const *decl) const {
  return constants_->second.addr_.at(decl);
}

void EmitIr::set_addr(ast::Declaration const *decl, ir::Reg r) {
  constants_->second.addr_[decl] = r;
}

void EmitIr::set_dispatch_table(ast::ExprPtr expr, ast::DispatchTable &&table) {
  constants_->second.dispatch_tables_.emplace(expr, std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

ast::DispatchTable const *EmitIr::dispatch_table_of(ast::ExprPtr expr) const {
  auto &table = constants_->second.dispatch_tables_;
  if (auto iter = table.find(expr); iter != table.end()) {
    return &iter->second;
  }
  return nullptr;
}
}  // namespace visitor
