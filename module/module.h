#ifndef ICARUS_MODULE_MODULE_H
#define ICARUS_MODULE_MODULE_H

#include <forward_list>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/node_hash_map.h"
#include "ast/scope.h"
#include "base/cast.h"
#include "base/guarded.h"
#include "base/macros.h"
#include "base/ptr_span.h"
#include "diagnostic/consumer/consumer.h"

namespace module {
struct Importer;

enum class Linkage { Internal, External };

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

  ast::ModuleScope const &scope() const { return scope_; }

  diagnostic::DiagnosticConsumer &diagnostic_consumer() {
    return *ASSERT_NOT_NULL(diagnostic_consumer_);
  }
  diagnostic::DiagnosticConsumer const &diagnostic_consumer() const {
    return *ASSERT_NOT_NULL(diagnostic_consumer_);
  }

  template <typename T, typename... Args>
  void set_diagnostic_consumer(Args &&... args) {
    diagnostic_consumer_ = std::make_unique<T>(std::forward<Args>(args)...);
  }

  void embed(BasicModule const &module) { scope_.embed(&module.scope_); }

  template <typename Iter>
  base::PtrSpan<ast::Node const> insert(Iter begin, Iter end) {
    size_t i = nodes_.size();
    while (begin != end) { nodes_.push_back(std::move(*begin++)); }
    return base::PtrSpan<ast::Node const>(nodes_.data() + i, nodes_.size() - i);
  }

  void InitializeNodes(base::PtrSpan<ast::Node> nodes);

 protected:
  virtual void ProcessNodes(base::PtrSpan<ast::Node const>,
                            diagnostic::DiagnosticConsumer &, Importer &) = 0;

 private:

  ast::ModuleScope scope_;
  std::vector<std::unique_ptr<ast::Node>> nodes_;
  std::unique_ptr<diagnostic::DiagnosticConsumer> diagnostic_consumer_;
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
