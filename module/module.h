#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "ast/scope/module.h"
#include "base/cast.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/consumer.h"
#include "frontend/source/source.h"

namespace module {
template <typename T>
struct ExtendedModule;

// BasicModule:
//
// Represents a unit of compilation, beyond which all intercommunication must be
// explicit.
struct BasicModule : base::Cast<BasicModule> {
  BasicModule();
  virtual ~BasicModule();

  // Pointers to modules are passed around, so moving a module is not safe.
  BasicModule(BasicModule &&) noexcept = delete;
  BasicModule &operator=(BasicModule &&) noexcept = delete;

  // Copying a module is implicitly disallowed as modules hold move-only types.
  // We explicitly delete them to improve error messages, and because even if
  // they were not implicitly deleted, we would not want modules to be copyable
  // anyway for reasons similar to those explaining why we disallow moves.
  BasicModule(BasicModule const &) = delete;
  BasicModule &operator=(BasicModule const &) = delete;

  void AppendNode(std::unique_ptr<ast::Node> node,
                  diagnostic::DiagnosticConsumer &diag);
  void AppendNodes(std::vector<std::unique_ptr<ast::Node>> nodes,
                   diagnostic::DiagnosticConsumer &diag);

  absl::Span<ast::Declaration const *const> declarations(
      std::string_view name) const;

  constexpr ast::ModuleScope const *scope() const { return &scope_; }

  void ProcessFromSource(frontend::Source *src,
                         diagnostic::DiagnosticConsumer &diag);

 protected:
  virtual void ProcessNodes(base::PtrSpan<ast::Node const>,
                            diagnostic::DiagnosticConsumer &) = 0;

 private:
  void InitializeNodes(base::PtrSpan<ast::Node> nodes);

  ast::ModuleScope scope_;
  absl::flat_hash_map<std::string_view, std::vector<ast::Declaration const *>>
      top_level_decls_;
  std::vector<std::unique_ptr<ast::Node>> nodes_;
};

// Returns a container of all declarations in this scope and in parent scopes
// with the given identifier.
std::vector<ast::Declaration const *> AllDeclsTowardsRoot(
    ast::Scope const *starting_scope, std::string_view id);

// Calls `fn` on each declaration in this scope and in parent scopes with the
// given identifier.
template <typename Fn>
bool ForEachDeclTowardsRoot(ast::Scope const *starting_scope,
                            std::string_view id, Fn fn) {
  for (auto scope_ptr = starting_scope; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    if (auto iter = scope_ptr->decls_.find(id);
        iter != scope_ptr->decls_.end()) {
      for (auto *decl : iter->second) {
        if (not fn(decl)) { return false; }
      }
    }

    for (auto const *mod : scope_ptr->embedded_modules_) {
      // TODO use the right bound constants? or kill bound constants?
      for (auto *decl : mod->declarations(id)) {
        // TODO what about transitivity for embedded modules?
        // New context will lookup with no constants.
        if (not fn(decl)) { return false; }
      }
    }
  }
  return true;
}

// Returns a container of all declaration with the given identifier that are in
// a scope directly related to this one (i.e., one of the scopes is an ancestor
// of the other, or is the root scope of an embedded module).
std::vector<ast::Declaration const *> AllAccessibleDecls(
    ast::Scope const *starting_scope, std::string_view id);

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
