#include "core/scope.h"

#include "module/module.h"

namespace core {

void Scope::InsertDecl(std::string const &id, ast::Declaration *decl) {
  decls_[id].push_back(decl);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    scope_ptr->child_decls_[id].push_back(decl);
  }
}

module::Module *Scope::module() {
  if (auto *ds = this->if_as<ModuleScope>()) { return ds->module_; }
  return parent->module();
}

module::Module const *Scope::module() const {
  if (auto *ds = this->if_as<ModuleScope>()) { return ds->module_; }
  return parent->module();
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
      if (auto *decl = mod->GetDecl(id)) {
        // New context will lookup with no constants.
        matching_decls.push_back(decl);
      }
    }
  }
  return matching_decls;
}

ExecScope::ExecScope(Scope *parent) : Scope(parent) {
  // If this scope is a FnScope it will be handled by the FnScope constructor.
  if (auto containing_fn_scope = parent->Containing<FnScope>()) {
    containing_fn_scope->innards_.push_back(this);
  }
}

}  // namespace core
