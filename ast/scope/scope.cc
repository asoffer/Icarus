#include "ast/scope/scope.h"
#include "base/log.h"

namespace ast {

void Scope::InsertDecl(std::string_view id, ast::Declaration *decl) {
  LOG("scope", "%p: %s => %p", this, id, decl);
  decls_[id].push_back(decl);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    scope_ptr->child_decls_[id].push_back(decl);
  }
}

}  // namespace ast
