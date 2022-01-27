#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/declaration.h"
#include "ast/module.h"
#include "ast/scope.h"
#include "base/cast.h"
#include "base/guarded.h"
#include "base/macros.h"
#include "type/qual_type.h"

namespace module {

enum class Linkage { Internal, External };

// Module:
//
// Represents a unit of compilation, beyond which all intercommunication must be
// explicit.
struct Module : base::Cast<Module> {
  virtual ~Module() {}

  struct SymbolInformation {
    type::QualType qualified_type;
    ir::CompleteResultBuffer value;
    // TODO: Remove this. It's only here as a temporary mechanism to work nicely
    // with generics until we have a decent cross-module solution for them. It
    // will only be populated for generics.
    ast::Declaration::Id const *id;
  };

  // Given a symbol `name`, returns a range of `SymbolInformation` describing
  // any exported symbols of that name in the module. The range of symbol
  // information has no ordering guarantees.
  virtual absl::Span<SymbolInformation const> Exported(
      std::string_view name) = 0;
};

// TODO: Rename this and merge with CompiledModule.
struct BasicModule : Module {
  explicit BasicModule() : module_(this) {}

  // Pointers to modules are passed around, so moving a module is not safe.
  BasicModule(BasicModule &&) noexcept = delete;
  BasicModule &operator=(BasicModule &&) noexcept = delete;

  // Copying a module is implicitly disallowed as modules hold move-only types.
  // We explicitly delete them to improve error messages, and because even if
  // they were not implicitly deleted, we would not want modules to be copyable
  // anyway for reasons similar to those explaining why we disallow moves.
  BasicModule(BasicModule const &) = delete;
  BasicModule &operator=(BasicModule const &) = delete;

  ast::Scope const &scope() const { return module_.body_scope(); }
  ast::Scope &scope() { return module_.body_scope(); }

  absl::Span<ast::Declaration::Id const *const> ExportedDeclarationIds(
      std::string_view name) const;

 protected:
  ast::Module module_;
  absl::flat_hash_map<std::string_view,
                      std::vector<ast::Declaration::Id const *>>
      exported_declarations_;
};

// Returns a container of all visible declarations in this scope with the given
// identifier. This means any declarations in the path to the ancestor
// function/jump, and any constant declarations above that.
std::vector<ast::Declaration::Id const *> AllVisibleDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id);

// Returns a container of all declaration ids with the given identifier that are
// in a scope directly related to this one (i.e., one of the scopes is an
// ancestor of the other, or is the root scope of an embedded module).
std::vector<ast::Declaration::Id const *> AllAccessibleDeclIds(
    ast::Scope const *starting_scope, std::string_view id);

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
