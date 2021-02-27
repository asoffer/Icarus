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
      if (scope_ptr->is_visibility_boundary()) { break; }
      child_decls_[id.name()].push_back(&id);
    }
  }
}

Scope::Scope(Scope *parent, bool executable)
    : parent_(parent), executable_(executable) {
  if (not parent_) { return; }
  for (Scope *s = parent_; s; s = s->parent_) {
    LOG("Scope", "%p", s);
    if (auto *fs = s->if_as<FnScope>()) {
      fs->insert_descendant(this);
      return;
    }
  }
  UNREACHABLE();
}

}  // namespace ast
