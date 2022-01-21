#include "ast/scope.h"

namespace ast {

void Scope::InsertDeclaration(ast::Declaration const *decl) {
  if (not(decl->flags() & Declaration::f_IsConst)) {
    ordered_non_constant_declarations_.push_back(decl);
  }
  for (auto const &id : decl->ids()) {
    if (id.name() == "move" or id.name() == "copy" or id.name() == "destroy") {
      continue;
    }
    LOG("Scope", "Inserting a declaration of `%s` into %p", id.name(), this);
    decls_[id.name()].push_back(&id);
    for (auto *scope_ptr = parent(); scope_ptr;
         scope_ptr       = scope_ptr->parent()) {
      if (kind_ == Kind::BoundaryExecutable) { break; }
      child_decls_[id.name()].push_back(&id);
    }
  }
}

Scope::Scope(Kind kind) : kind_(kind) {
  executable_descendants_.push_back(this);
}

Scope::Scope(module::BasicModule *module)
    : parent_(reinterpret_cast<uintptr_t>(module) | 1),
      kind_(Kind::BoundaryExecutable) {
  executable_descendants_.push_back(this);
}

void Scope::set_parent(Scope *p) {
  ASSERT(parent_ == 0u);
  parent_  = reinterpret_cast<uintptr_t>(p);
  Scope *s = this;
  if (s->kind() != Kind::BoundaryExecutable) {
    p->ordered_non_constant_declarations_.push_back(this);
  }
  while (s->kind() != Kind::BoundaryExecutable) { s = s->parent(); }
  if (s != this) { s->executable_descendants_.push_back(this); }
}

}  // namespace ast
