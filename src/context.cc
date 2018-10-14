#include "context.h"

type::Type const *Context::type_of(AST::Expression const *expr) const {
  return mod_->type_of(bound_constants_, expr);
}

void Context::set_type(AST::Expression const *expr, type::Type const *t) {
  return mod_->set_type(bound_constants_, expr, t);
}

void Context::set_addr(AST::Declaration *decl, IR::Register r) {
  mod_->addr_[bound_constants_][decl] = r;
}

IR::Register Context::addr(AST::Declaration *decl) const {
  return mod_->addr(bound_constants_, decl);
}
