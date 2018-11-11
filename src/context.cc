#include "context.h"

type::Type const *Context::type_of(ast::Expression const *expr) const {
  auto *bc     = &bound_constants_;
  auto *result = mod_->type_of(bound_constants_, expr);
  if (result) { return result; }
  if (!parent_) { return nullptr; }
  return parent_->type_of(expr);
}

void Context::set_type(ast::Expression const *expr, type::Type const *t) {
  return mod_->set_type(bound_constants_, expr, t);
}

void Context::set_addr(ast::Declaration *decl, ir::Register r) {
  mod_->addr_[bound_constants_][decl] = r;
}

ir::Register Context::addr(ast::Declaration *decl) const {
  return mod_->addr(bound_constants_, decl);
}
