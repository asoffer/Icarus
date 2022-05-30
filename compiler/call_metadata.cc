#include "compiler/call_metadata.h"

#include "type/block.h"
#include "type/function.h"
#include "type/generic.h"
#include "type/struct.h"

namespace compiler {
namespace {

// Finds all expressions bound to the identifier `name` which are either visible
// in the scope `primary`, or exported from the modules contained in `modules`.
absl::flat_hash_set<CallMetadata::callee_locator_t> Overloads(
    std::string_view name, ast::Scope const *primary,
    absl::flat_hash_set<module::Module *> const &modules) {
  absl::flat_hash_set<CallMetadata::callee_locator_t> overloads;

  bool only_constants = false;

  for (ast::Declaration::Id const &id :
       primary->visible_ancestor_declaration_id_named(name)) {
    overloads.insert(&static_cast<ast::Expression const &>(id));
  }

  for (ast::Scope const &s : primary->ancestors()) {
    for (auto *mod : s.embedded_modules()) {
      for (auto const &symbol : mod->Exported(name)) {
        if (symbol.qualified_type.constant()) { overloads.insert(&symbol); }
      }
    }
  }

  for (auto *mod : modules) {
    if (mod == &primary->module()) { continue; }
    for (auto const &symbol : mod->Exported(name)) {
      overloads.insert(&symbol);
    }
  }
  return overloads;
}

absl::flat_hash_set<CallMetadata::callee_locator_t> Overloads(
    std::string_view name, module::Module *module) {
  absl::flat_hash_set<CallMetadata::callee_locator_t> overloads;
  for (auto const &symbol : module->Exported(name)) {
    overloads.insert(&symbol);
  }
  return overloads;
}

}  // namespace

CallMetadata::CallMetadata(std::string_view name, ast::Scope const *primary,
                           absl::flat_hash_set<module::Module *> const &modules)
    : CallMetadata(Overloads(name, primary, modules)) {}

CallMetadata::CallMetadata(std::string_view name, module::Module *mod)
    : CallMetadata(Overloads(name, mod)) {}

}  // namespace compiler
