#include "ast/scope/scope.h"
#include "base/log.h"

namespace ast {
struct Declaration;

void Scope::InsertDecl(std::string_view id, ast::Declaration *decl) {
  LOG("scope", "%p: %s => %p", this, id, decl);
  decls_[id].push_back(decl);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    if (scope_ptr->is_visibility_boundary()) { break; }
    child_decls_[id].push_back(decl);
  }
}

}  // namespace ast
