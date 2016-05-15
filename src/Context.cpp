#include "Context.h"

Context Context::spawn() {
  Context ctx;
  ctx.parent_ = this;
  return ctx;
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


