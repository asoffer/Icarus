#include "Context.h"
#include "AST.h"

Context Context::GlobalContext;

EPtr Context::get(IdPtr idptr) {
  auto iter = bindings_.find(idptr);
  if (iter == bindings_.end()) {
    if (parent_ == nullptr) {
      return nullptr;
    }

    return parent_->get(idptr);
  }

  return iter->second;
}

Context Context::spawn() {
  Context ctx;
  ctx.parent_ = this;
  return ctx;
}
void Context::bind(EPtr eptr, IdPtr idptr) {
  bindings_[idptr] = eptr;
}

void Context::set_return_value(Value v) {
  has_ret_ = true;
  ret_val_ = v;
}
