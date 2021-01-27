#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <forward_list>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "absl/synchronization/notification.h"
#include "ast/scope.h"
#include "base/cast.h"
#include "base/expected.h"
#include "base/guarded.h"
#include "base/macros.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/consumer.h"
#include "diagnostic/consumer/streaming.h"
#include "module/importer.h"

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

  void AppendNodes(std::vector<std::unique_ptr<ast::Node>> nodes,
                   diagnostic::DiagnosticConsumer &diag, Importer &importer);

  constexpr ast::ModuleScope &scope() { return scope_; }
  constexpr ast::ModuleScope const &scope() const { return scope_; }

 protected:
  virtual void ProcessNodes(base::PtrSpan<ast::Node const>,
                            diagnostic::DiagnosticConsumer &, Importer &) = 0;

 private:
  void InitializeNodes(base::PtrSpan<ast::Node> nodes);

  ast::ModuleScope scope_;
  std::vector<std::unique_ptr<ast::Node>> nodes_;
};

// Returns a container of all visible declarations in this scope  with the given
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
