#include "ast/scope.h"

namespace ast {

void Scope::InsertDeclaration(ast::Declaration const *decl) {
  for (auto const &id : decl->ids()) {
    if (id.name() == "move" or id.name() == "copy" or id.name() == "destroy") {
      continue;
    }
    decls_[id.name()].push_back(&id);
    for (auto *scope_ptr = parent(); scope_ptr;
         scope_ptr       = scope_ptr->parent()) {
      if (scope_ptr->is_visibility_boundary()) { break; }
      child_decls_[id.name()].push_back(&id);
    }
  }
}

}  // namespace ast
