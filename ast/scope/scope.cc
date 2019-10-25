#include "ast/scope/scope.h"

#include "ast/scope/exec.h"
#include "ast/scope/fn.h"
#include "module/module.h"

namespace ast {

void Scope::InsertDecl(std::string_view id, ast::Declaration *decl) {
  decls_[id].push_back(decl);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    scope_ptr->child_decls_[id].push_back(decl);
  }
}

std::vector<ast::Declaration const *> Scope::AllDeclsWithId(
    std::string_view id) const {
  std::vector<ast::Declaration const *> matching_decls;
  for (auto scope_ptr = this; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    if (auto iter = scope_ptr->decls_.find(id);
        iter != scope_ptr->decls_.end()) {
      for (auto *decl : iter->second) { matching_decls.push_back(decl); }
    }

    for (auto const *mod : scope_ptr->embedded_modules_) {
      // TODO use the right bound constants? or kill bound constants?
      for (auto *decl : mod->declarations(id)) {
        // TODO what about transitivity for embedded modules?
        // New context will lookup with no constants.
        matching_decls.push_back(decl);
      }
    }
  }
  return matching_decls;
}

}  // namespace ast
