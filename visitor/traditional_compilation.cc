#include "visitor/traditional_compilation.h"

#include "ast/ast.h"
#include "ast/expr_ptr.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/results.h"
#include "visitor/emit_ir.h"
#include "visitor/verify_type.h"

namespace visitor {

TraditionalCompilation::TraditionalCompilation(Module *mod)
    : mod_(mod), bldr_(ir::GetBuilder()) {
  constants_ = &mod_->dep_data_.front();
}

VerifyResult const *TraditionalCompilation::prior_verification_attempt(
    ast::ExprPtr expr) {
  auto const &map = constants_->second.verify_results_;
  if (auto iter = map.find(expr); iter != map.end()) { return &iter->second; }
  return nullptr;
}

base::Tagged<ir::Addr, ir::Reg> TraditionalCompilation::Alloca(
    type::Type const *t) {
  return builder().function()->Alloca(t);
}

base::Tagged<ir::Addr, ir::Reg> TraditionalCompilation::TmpAlloca(
    type::Type const *t) {
  auto reg = Alloca(t);
  temporaries_to_destroy_->emplace_back(reg, t);
  return reg;
}

type::Type const *TraditionalCompilation::type_of(ast::Expression const *expr) const {
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

void TraditionalCompilation::set_addr(ast::Declaration const *decl,
                                      ir::Reg addr) {
  constants_->second.addr_[decl] = addr;
}
visitor::VerifyResult TraditionalCompilation::set_result(
    ast::ExprPtr expr, visitor::VerifyResult r) {
  constants_->second.verify_results_.emplace(expr, r);
  return r;
}

ir::Reg TraditionalCompilation::addr(ast::Declaration const *decl) const {
  return constants_->second.addr_.at(decl);
}

void TraditionalCompilation::set_dispatch_table(ast::ExprPtr expr,
                                                ast::DispatchTable &&table) {
  constants_->second.dispatch_tables_.emplace(expr, std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

std::pair<ConstantBinding, DependentData>
    *TraditionalCompilation::insert_constants(
        ConstantBinding const &constant_binding) {
  auto *pair = mod_->insert_constants(constant_binding);
  for (auto const &[decl, binding] : constant_binding.keys_) {
    pair->second.verify_results_.emplace(
        ast::ExprPtr(decl), visitor::VerifyResult::Constant(binding.type_));
  }
  return pair;
}

void TraditionalCompilation::set_jump_table(ast::ExprPtr jump_expr,
                                            ast::ExprPtr node,
                                            ast::DispatchTable &&table) {
  constants_->second.jump_tables_.emplace(std::pair{jump_expr, node},
                                          std::move(table));
  // TODO in some situations you may be trying to set the dispatch table more
  // than once. This has come up with generic structs and you should
  // investigate.
  //
  // static_cast<void>(iter);
  // ASSERT(success) << expr;
}

void TraditionalCompilation::set_pending_module(ast::Import const *import_node,
                                                core::PendingModule mod) {
  constants_->second.imported_module_.emplace(import_node, std::move(mod));
}

ast::DispatchTable const *TraditionalCompilation::dispatch_table(
    ast::ExprPtr expr) const {
  auto &table = constants_->second.dispatch_tables_;
  if (auto iter = table.find(expr); iter != table.end()) {
    return &iter->second;
  }
  return nullptr;
}

core::PendingModule *TraditionalCompilation::pending_module(
    ast::Import const *import_node) const {
  if (auto iter = constants_->second.imported_module_.find(import_node);
      iter != constants_->second.imported_module_.end()) {
    return &iter->second;
  }
  return nullptr;
}
}  // namespace visitor
