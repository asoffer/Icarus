#include "Context.h"
#include "AST.h"

Context Context::GlobalContext;

Context::Value Context::get(IdPtr idptr) {
  auto iter = bindings_.find(idptr);
  if (iter == bindings_.end()) {
    return parent_ == nullptr ? nullptr : parent_->get(idptr);
  }

  return iter->second;
}

Context Context::spawn() {
  Context ctx;
  ctx.parent_ = this;
  return ctx;
}
void Context::bind(Context::Value v, IdPtr idptr) {
  bindings_.emplace(std::make_pair(idptr, v));
}

void Context::set_return_value(Value v) {
  has_ret_ = true;
  ret_val_ = v;

  // TODO this is a hacky way to ensure all scopes have a return value all the way up
  if (parent_ != nullptr) {
    parent_->set_return_value(v);
  }
}
