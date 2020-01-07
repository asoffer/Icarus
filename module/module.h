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

  void AppendNode(std::unique_ptr<ast::Node> node);
  void AppendNodes(std::vector<std::unique_ptr<ast::Node>> nodes);

  absl::Span<ast::Declaration const *const> declarations(
      std::string_view name) const;

  constexpr ast::ModuleScope const *scope() const { return &scope_; }

  void ProcessFromSource(frontend::Source *src);

 protected:
  virtual void ProcessNodes(base::PtrSpan<ast::Node const>) = 0;

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

// Returns a container of all declaration with the given identifier that are in
// a scope directly related to this one (i.e., one of the scopes is an ancestor
// of the other, or is the root scope of an embedded module).
std::vector<ast::Declaration const *> AllAccessibleDecls(
    ast::Scope const *starting_scope, std::string_view id);

}  // namespace module

#endif  // ICARUS_MODULE_MODULE_H
