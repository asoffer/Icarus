#include "module/module.h"

#include "absl/algorithm/container.h"
#include "ast/ast.h"

namespace module {

absl::Span<ast::Declaration::Id const *const>
BasicModule::ExportedDeclarationIds(std::string_view name) const {
  auto iter = exported_declarations_.find(name);
  if (iter == exported_declarations_.end()) { return {}; }

  // TODO: handle exported embedded modules here too.
  return iter->second;
}

// TODO: Add a version of this function that also gives the declarations that
// are inaccessible. Particularly interesting would be the case of an overlaod
// set mixing constant and non-constants. It should also be an error to
// reference that when you're only able to see some of the name.
std::vector<ast::Declaration::Id const *> AllVisibleDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id_name) {
  std::vector<ast::Declaration::Id const *> ids;
  bool only_constants = false;

  auto fn = [&](ast::Declaration::Id const *id) {
    if (not only_constants or
        (id->declaration().flags() & ast::Declaration::f_IsConst)) {
      ids.push_back(id);
    }
    return true;
  };

  for (ast::Scope const &s : starting_scope->ancestors()) {
    if (auto iter = s.decls_.find(id_name); iter != s.decls_.end()) {
      // TODO: Support multiple declarations
      for (auto const *id : iter->second) { fn(id); }
    }

    for (auto const *mod : s.embedded_modules()) {
      for (auto const *id : mod->ExportedDeclarationIds(id_name)) {
        if (only_constants or
            (id->declaration().flags() & ast::Declaration::f_IsConst)) {
          ids.push_back(id);
        }
      }
    }

    if (s.is<ast::FnScope>()) { only_constants = true; }
  }
  return ids;
}

std::vector<ast::Declaration::Id const *> AllAccessibleDeclIds(
    ast::Scope const *starting_scope, std::string_view id_name) {
  std::vector<ast::Declaration::Id const *> decl_iters =
      module::AllVisibleDeclsTowardsRoot(starting_scope, id_name);
  auto child_decls = starting_scope->VisibleChildren(id_name);
  decl_iters.insert(decl_iters.end(), child_decls.begin(), child_decls.end());
  return decl_iters;
}

}  // namespace module
