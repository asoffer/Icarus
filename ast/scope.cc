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

Scope::Scope(module::BasicModule *module, bool executable)
    : parent_(reinterpret_cast<uintptr_t>(module) | 1),
      executable_(executable) {}

Scope::Scope(Kind kind, Scope *parent_scope, bool executable)
    : parent_(reinterpret_cast<uintptr_t>(parent_scope)),
      executable_(executable) {
  for (Scope *s = parent(); s; s = s->parent()) {
    LOG("Scope", "%p", s);
    if (auto *fs = s->if_as<FnScope>()) {
      fs->insert_descendant(this);
      return;
    }
  }
  UNREACHABLE();
    }

Scope::Scope(Scope *parent_scope, bool executable)
    : Scope(Scope::Kind::Executable, parent_scope, executable) {}

}  // namespace ast
