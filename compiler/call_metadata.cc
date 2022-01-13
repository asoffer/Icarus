#include "compiler/call_metadata.h"

namespace compiler {
namespace {

// Finds all expressions bound to the identifier `name` which are either visible
// in the scope `primary`, or exported from the modules contained in `modules`.
absl::flat_hash_set<ast::Expression const *> Overloads(
    std::string_view name, ast::Scope const *primary,
    absl::flat_hash_set<module::BasicModule const *> const &modules) {
  auto exprs = module::AllVisibleDeclsTowardsRoot(primary, name);
  absl::flat_hash_set<ast::Expression const *> overloads(exprs.begin(),
                                                         exprs.end());
  exprs.clear();
  for (auto const *mod : modules) {
    auto ids = mod->ExportedDeclarationIds(name);
    for (auto const *decl_id : ids) { overloads.insert(decl_id); }
  }
  return overloads;
}

}  // namespace

CallMetadata::CallMetadata(
    std::string_view name, ast::Scope const *primary,
    absl::flat_hash_set<module::BasicModule const *> const &modules)
    : CallMetadata(Overloads(name, primary, modules)) {}

}  // namespace compiler
