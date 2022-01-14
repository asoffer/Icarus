#include "ast/scope.h"

namespace ast {

void Scope::InsertDeclaration(ast::Declaration const *decl) {
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

Scope::Scope(module::BasicModule *module)
    : parent_(reinterpret_cast<uintptr_t>(module) | 1),
      kind_(Kind::BoundaryExecutable) {
  insert_descendant(this);
}

void Scope::set_parent(Scope *p) {
  ASSERT(parent_ == 0u);
  parent_ = reinterpret_cast<uintptr_t>(p);
  if (kind() == Kind::BoundaryExecutable) { return; }
  for (Scope *s = this; s; s = s->parent()) {
    s->insert_descendant(this);
    if (s->kind_ == Kind::BoundaryExecutable) { return; }
  }
  UNREACHABLE();
}

}  // namespace ast
