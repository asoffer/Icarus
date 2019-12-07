#include "ast/scope/scope.h"

namespace ast {

void Scope::InsertDecl(std::string_view id, ast::Declaration *decl) {
  DEBUG_LOG("scope")(this, ": ", id, " => ", decl);
  decls_[id].push_back(decl);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    scope_ptr->child_decls_[id].push_back(decl);
  }
}

std::vector<ast::Declaration const *> Scope::AllAccessibleDecls(
    std::string_view id) const {
  std::vector<ast::Declaration const *> decls = AllDeclsTowardsRoot(id);
  if (auto iter = child_decls_.find(id); iter != child_decls_.end()) {
    decls.reserve(decls.size() + iter->second.size());
    for (Declaration const *decl : iter->second) { decls.push_back(decl); }
  }

  return decls;
}

std::vector<ast::Declaration const *> Scope::AllDeclsTowardsRoot(
    std::string_view id) const {
  std::vector<ast::Declaration const *> decls;
  for (auto scope_ptr = this; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    if (auto iter = scope_ptr->decls_.find(id);
        iter != scope_ptr->decls_.end()) {
      for (auto *decl : iter->second) { decls.push_back(decl); }
    }

    for (auto const *mod : scope_ptr->embedded_modules_) {
      DEBUG_LOG("AllDeclsTowardsRoot")(this, " ", mod);
      //   // TODO use the right bound constants? or kill bound constants?
      // for (auto *decl : mod->declarations(id)) {
      //   DEBUG_LOG("AllDeclsTowardsRoot")("   ", id);
      //   // TODO what about transitivity for embedded modules?
      //   // New context will lookup with no constants.
      //   decls.push_back(decl);
      // }
    }
  }

  return decls;
}

}  // namespace ast
