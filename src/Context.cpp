#include "Context.h"
#include "AST.h"

Context::Value Context::get(AST::Identifier *idptr) {
  auto iter = bindings_.find(idptr);
  if (iter == bindings_.end()) {
    return !parent_ ? nullptr : parent_->get(idptr);
  }

  return iter->second;
}

Context Context::spawn() {
  Context ctx;
  ctx.parent_ = this;
  return ctx;
}
void Context::bind(Context::Value v, AST::Identifier *idptr) {
  bindings_.emplace(idptr, v);
}

void Context::set_return_value(Value v) {
  has_ret_ = true;
  ret_val_ = v;

  // TODO this is a hacky way to ensure all scopes have a return value all the way up
  if (parent_ != nullptr) {
    parent_->set_return_value(v);
  }
}

bool operator==(Context::Value lhs, Context::Value rhs) {
  // We choose double because it's the largest possibility,
  // so we don't have any problems with data slicing
  return lhs.as_real == rhs.as_real;
}

bool operator!=(Context::Value lhs, Context::Value rhs) {
  return !(lhs == rhs);
}


