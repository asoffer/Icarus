#include "ast/scope/scope.h"

namespace ast {

void Scope::InsertDeclaration(Declaration::const_iterator iter) {
  decls_[iter->id()].push_back(iter);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    if (scope_ptr->is_visibility_boundary()) { break; }
    child_decls_[iter->id()].push_back(iter);
  }
}

}  // namespace ast
