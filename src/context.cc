#include "context.h"
#include "ast/expression.h"

type::Type const *Context::type_of(ast::Expression const *expr) const {
  if (auto *result = mod_->type_of(bound_constants_, expr)) { return result; }
  if (parent_) { return parent_->type_of(expr); }

  for (Module const *mod : mod_->global_->embedded_modules_) {
    auto bc_iter = mod->types_.find(bound_constants_);
    if (bc_iter == mod->types_.end()) { continue; }
    auto iter = bc_iter->second.data_.find(expr);
    if (iter != bc_iter->second.data_.end()) { return iter->second; }
  }
  return nullptr;
}

type::Type const *Context::set_type(ast::Expression const *expr,
                                    type::Type const *t) {
  return mod_->set_type(bound_constants_, expr, t);
}

void Context::set_addr(ast::Declaration *decl, ir::Register r) {
  mod_->addr_[bound_constants_][decl] = r;
}

ir::Register Context::addr(ast::Declaration *decl) const {
  return mod_->addr(bound_constants_, decl);
}
