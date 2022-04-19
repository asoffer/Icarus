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
  for (ast::Scope const &s : primary->ancestors()) {
    if (auto iter = s.decls_.find(name); iter != s.decls_.end()) {
      // TODO: Support multiple declarations
      for (auto const *id : iter->second) {
        if (not only_constants or
            (id->declaration().flags() & ast::Declaration::f_IsConst)) {
          overloads.insert(static_cast<ast::Expression const *>(id));
        }
      }
    }

    for (auto *mod : s.embedded_modules()) {
      for (auto const &symbol : mod->Exported(name)) {
        if (not only_constants or symbol.qualified_type.constant()) {
          overloads.insert(&symbol);
        }
      }
    }

    if (s.kind() == ast::Scope::Kind::BoundaryExecutable) {
      only_constants = true;
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
